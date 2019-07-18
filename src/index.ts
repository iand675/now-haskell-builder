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
    createLambda,
    // Route,
    BuildOptions,
    FileFsRef,
    // Config,
} from '@now/build-utils';
import installHaskell from './install-haskell';

export const version = 2;



export async function build({
    files,
    entrypoint,
    workPath,
    config,
    meta = {},
}: BuildOptions) {

    console.log(entrypoint);
    if (!meta.isDev) {
        await installHaskell(String(config.stack));
    }

    // TODO find targets in stack that correspond to entrypoints
    // TODO inject needed targets if missing
    // TODO inject needed libs into cabal file if missing
    const downloadedFiles = await download(files, workPath, meta);
    const entrypointDirname = path.dirname(downloadedFiles[entrypoint].fsPath);

    try {
        await execa(
            'stack',
            [
                'install',
                '--local-bin-path',
                workPath
            ].concat(config.debug ? ['--fast'] : []),
            {
                // env: rustEnv,
                cwd: entrypointDirname,
                stdio: 'inherit',
            }
        );
    } catch (err) {
        console.error('failed to `cargo build`');
        throw err;
    }

    let routes: Object = [];
    let watch: Array<String> = [];
    let extraFiles = await glob('**', workPath)
    let lambda = await createLambda({
        files: {
            ...extraFiles,
            bootstrap: new FileFsRef({
                mode: 0o755,
                fsPath: path.join(workPath, 'test-raw-exe')
            }),
        },
        handler: 'bootstrap',
        runtime: 'provided',
        environment: {}
    });

    let output = {
        [entrypoint]: lambda
    };

    return { routes, watch, output };
}

export { shouldServe } from '@now/build-utils';
