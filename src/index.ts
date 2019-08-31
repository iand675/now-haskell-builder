// import spawn from 'cross-spawn';
import execa from 'execa';
import path from 'path';
import {
    glob,
    download,
    // runNpmInstall,
    // runPackageJsonScript,
    // runShellScript,
    // getNodeVersion,
    // getSpawnOptions,
    runShellScript,
    debug,
    createLambda,
    // Route,
    BuildOptions,
    FileFsRef,
    // Config,
} from '@now/build-utils';
import fs from "fs-extra";
import findUp from 'find-up';

import installHaskell from './install-haskell';
import YAML from 'yaml';

export const version = 2;

interface Scan {
	  type: string,
    data: any
}

async function runUserScripts(entrypoint: string) {
	  const entryDir = path.dirname(entrypoint);
	  const buildScriptPath = path.join(entryDir, "build.sh");
	  const buildScriptExists = await fs.pathExists(buildScriptPath);

	  if (buildScriptExists) {
		    console.log("running `build.sh`...");
		    await runShellScript(buildScriptPath);
	  }
}

/*
  For cabal or yaml entrypoints, just take the specified executable or the only existing one.

  For (l)hs files, figure out if they're exported from a library and use that in tandem with adding to a cabal file.
  Otherwise, generate a cabal file and wire it into the package.yaml file.

  Builds should happen in a
  */
export async function build({
    files,
    entrypoint,
    workPath,
    config,
    meta = {},
}: BuildOptions) {
    console.log(entrypoint, workPath, config, meta);

    const lambdasOut = path.join(workPath, '/dist/now-2/hs-mod-lambdas')
    const binsOut = path.join(workPath, '/dist/now-2/bins')

    await fs.mkdirp(lambdasOut);
    await fs.mkdirp(binsOut);

    await runUserScripts(entrypoint);

    if (!meta.isDev) {
        await installHaskell(String(config.stack));
    }

    // TODO find targets in stack that correspond to entrypoints
    // TODO inject needed targets if missing
    // TODO inject needed libs into cabal file if missing
    const downloadedFiles = await download(files, workPath, meta);
    const entrypointDirname = path.dirname(downloadedFiles[entrypoint].fsPath);

    debug('Building module-scanner and any preexisting binaries');
    try {
        await execa(
            'stack',
            [
                'install',
                'now-haskell',
                '--local-bin-path',
                binsOut,
            ].concat(meta.isDev ? ['--fast'] : []),
            {
                // env: rustEnv,
                cwd: entrypointDirname,
                stdio: 'pipe',
            }
        );
    } catch (err) {
        console.error('failed to `stack build`');
        throw err;
    }

    var haskellStructure: Scan;
    var jsonStr = '';

    // try {
    debug('Scanning modules', path.join(binsOut, 'module-scanner'), entrypointDirname);
    const child = execa(
        path.join(binsOut, 'module-scanner'),
        [entrypoint]
    );

    if (child.stdout) {
        child.stdout.on('data', (buf) => jsonStr += buf.toString());
    } else {
        throw new Error('Unable to read stdout');
    }

    await child;
    haskellStructure = JSON.parse(jsonStr);
    // console.log(haskellStructure);


    let watchFs = await glob('!(.)**/@(*.?(l)hs|package.yaml|package.yml|stack.yaml|stack.yml|*.cabal)', workPath);
    let watch = Object.keys(watchFs);

    /*
      TODO if we have a module:
      - generate new project into dist/now-2/hs-mod-lambdas
      - use config to add additional stanzas to cabal
      - copy stack.yaml into hs-mod-lambdas project and relativize local dirs
      - STACK_YAML=dist/now-2/hs-mod-lambdas/$PROJECT_NAME/stack.yaml
    */
    if (haskellStructure.type == "module") {
        debug('Entrypoint determined to be module');
        const genPkgName = haskellStructure.data.package.library + '-' + haskellStructure.data.analysis.moduleName.replace('.', '-');
        const pkgPath = path.join(lambdasOut, genPkgName);

        // {-# START_FILE {{name}}.cabal #-}

        const cabalTemplate = `name:                ${genPkgName}
version:             0.1.0.0
build-type:          Simple
cabal-version:       >=1.10

executable ${genPkgName}
    hs-source-dirs:      src
    main-is:             Main.hs
    default-language:    Haskell2010
    build-depends:       base >= 4.7 && < 5,
                         aws-lambda-runtime,
                         ${haskellStructure.data.package.library}
`;
        const mainTemplate = `
module Main where
import Zeit.Now
import ${haskellStructure.data.analysis.moduleName}

main :: IO ()
main = runloop handler
`;

        await fs.mkdirp(pkgPath);
        await fs.mkdirp(path.join(pkgPath, 'src'));
        await fs.promises.writeFile(path.join(pkgPath, genPkgName + '.cabal'), cabalTemplate);
        await fs.promises.writeFile(path.join(pkgPath, 'src', 'Main.hs'), mainTemplate);
            /*
            await execa(
                'stack',
                ['new', genPkgName, modLambdaTplOut,
                 "-p", 'handler-module:' + haskellStructure.data.analysis.moduleName,
                 "-p", 'handler-lib:' + haskellStructure.data.package.library,
                 // '--omit-packages', 'aws-lambda-runtime',
                 // '--verbosity', 'debug',
                ],
                {
                    cwd: lambdasOut
                }
            );
            */

        let stackYamlLoc = await findUp('stack.yaml', {
            cwd: workPath
        });

        if (!stackYamlLoc) {
            throw new Error('Must have stack.yaml in project');
        }

        let stackDir = path.dirname(stackYamlLoc);
        let stackYaml = YAML.parse(await fs.promises.readFile(stackYamlLoc, 'utf-8'))

        // console.log(stackYaml.packages);
        for (var i = 0; i < stackYaml.packages.length; i++) {
            let resolved = path.resolve(stackDir, stackYaml.packages[i]);
            // console.log(resolved);
            stackYaml.packages[i] = resolved;
        }

        stackYaml.packages.push(pkgPath);

        await fs.promises.writeFile(path.join(pkgPath, 'stack.yaml'), YAML.stringify(stackYaml));

        debug('Looking for stack.yaml at ', path.join(pkgPath, 'stack.yaml'));
        await execa(
            'stack',
            [
                'install',
                '--local-bin-path',
                binsOut
            ].concat(meta.isDev ? ['--fast'] : []),
            {
                env: {
                    STACK_YAML: path.join(pkgPath, 'stack.yaml')
                },
                cwd: pkgPath,
                stdio: 'pipe',
            }
        );

        let lambda = await createLambda({
            files: {
                // ...extraFiles,
                bootstrap: new FileFsRef({
                    mode: 0o755,
                    fsPath: path.join(binsOut, genPkgName)
                }),
            },
            handler: 'bootstrap',
            runtime: 'provided',
            environment: {}
        });


        let output = {
            [entrypoint]: lambda
        }

        let out = { watch, output };
        return out;
    } else {
        // TODO routes isn't really documented. What does it do?
        // let routes: Object = {};
        // let extraFiles = await glob('**', workPath);

        // let output: any = {};

        debug('Entrypoint determined to be package');
        let exePath: string = '';
        if (haskellStructure.data.executables.length == 1) {
            exePath = haskellStructure.data.executables[0];
        } else if (config.entrypoint) {
            exePath = config.entrypoint.toString();
        } else {
            throw new Error('Need at least one executable path currently');
        }

        let lambda = await createLambda({
            files: {
                // ...extraFiles,
                bootstrap: new FileFsRef({
                    mode: 0o755,
                    fsPath: path.join(workPath, exePath)
                }),
            },
            handler: 'bootstrap',
            runtime: 'provided',
            environment: {}
        });


        let output = {
            [entrypoint]: lambda
        }

        await Promise.all(haskellStructure.data.executables.map(async (exe: string) => {
            let lambda = await createLambda({
                files: {
                    // ...extraFiles,
                    bootstrap: new FileFsRef({
                        mode: 0o755,
                        fsPath: path.join(workPath, exe)
                    }),
                },
                handler: 'bootstrap',
                runtime: 'provided',
                environment: {}
            });

            output[exe] = lambda;
        }));

        let out = { watch, output };
        return out;
    }

    /*
      } catch (err) {
      console.error('Unable to scan haskell project due to failed build.')
      }
    */

}

export { shouldServe } from '@now/build-utils';
