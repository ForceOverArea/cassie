import { readFileSync, writeFileSync, lstatSync, mkdirSync, PathLike } from 'node:fs';
import { homedir } from 'node:os';
import { argv, chdir, cwd, stdin, stdout } from 'node:process';
import { createInterface } from 'node:readline/promises';
import { WASI } from 'node:wasi'; // use runno since Node's WASI implementation gets indigestion from Haskell's WASM
import ghc_wasm_jsffi from './wasi/ghc_wasm_jsffi.js';

// Constants 
const CASSIE_STDIN_IF = createInterface({ input: stdin, output: stdout });
// const CASSIE_WASM_STDIO_CONFIG: Partial<WASIContextOptions> = { args: argv, stdout: console.log };
const WASM_REACTOR_PATH = `${import.meta.dirname}/wasi/reactor.wasm`;

// Singletons/mutable data
const jsffiExports = {};
const wasi = new WASI({ 
  version: 'preview1', 
  args: argv,
});

// Make interface functions available for Haskell
globalThis.fs_readFileSync = (path: PathLike) => readFileSync(path, { encoding: 'utf-8' }).toString();
globalThis.fs_writeFileSync = (path: PathLike, contents: string) => writeFileSync(path, contents, { encoding: 'utf-8' });
globalThis.fs_lstat_isFile = (path: PathLike) => lstatSync(path).isFile();
globalThis.fs_mkdir = (rcsv: boolean, path: PathLike) => mkdirSync(path, { recursive: Boolean(rcsv) });
globalThis.os_homedir = homedir;
globalThis.process_cwd = cwd;
globalThis.process_chdir = chdir;
globalThis.readlineIF_question = () => CASSIE_STDIN_IF.question('>>> ');

/**
 * Entry point for main script
 */
async function main() {
  let caughtErr = undefined;
  try {
    const wasmBuffer = readFileSync(WASM_REACTOR_PATH);
    const importObject = Object.assign(
      { ghc_wasm_jsffi: ghc_wasm_jsffi(jsffiExports) },
      wasi.getImportObject()
    );
    const wasm = await WebAssembly.instantiate(wasmBuffer, importObject);

    Object.assign(jsffiExports, wasm.instance.exports);
    wasi.initialize(wasm);

    await (wasm.instance.exports.mainJS as any as () => Promise<void>)();
  } catch (err) {
    caughtErr = err;
  } finally {
    // START cleanup
    CASSIE_STDIN_IF.close();
    // END cleanup
    if ('undefined' !== typeof caughtErr) {
      throw caughtErr;
    }
  }
}

await main();
