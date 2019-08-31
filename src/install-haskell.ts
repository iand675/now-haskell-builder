import execa from 'execa';

async function installStack(version: string = '2.1.3'): Promise<void> {
    console.log('downloading the Haskell toolchain');

    try {
        await execa.shell(`curl -sSL https://get.haskellstack.org/ | STACK_VERSION="${version}" sh -s - -f`);
        console.log('Successfully installed stack.')
    } catch (err) {
        throw new Error(`Failed to install Haskell via stack: ${err.message}`);
    }
}

export default async (version?: string): Promise<void> => {
    await installStack(version);
};
