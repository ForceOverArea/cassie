import { readFileSync, writeFileSync, lstatSync, mkdirSync, PathLike } from 'node:fs';
import { homedir } from 'node:os';
import { argv, chdir, cwd, env } from 'node:process';
import { WASI, WASIContextOptions } from '@runno/wasi'; // use runno since Node's WASI implementation gets indigestion from Haskell's WASM
import ghc_wasm_jsffi from './wasi/ghc_wasm_jsffi.js';

// Make interface functions available for Haskell
globalThis.fs_readFileSync = (path: PathLike) => readFileSync(path).toString();
globalThis.fs_writeFileSync = (path: PathLike, contents: string) => writeFileSync(path, contents);
globalThis.fs_lstat_isFile = (path: PathLike) => lstatSync(path).isFile();
globalThis.fs_mkdir = (recursive: boolean, path: PathLike) => mkdirSync(path, { recursive });
globalThis.os_homedir = homedir;
globalThis.process_cwd = cwd;
globalThis.process_chdir = chdir;

const WASM_REACTOR_PATH = `${import.meta.dirname}/wasi/reactor.wasm`;
const CASSIE_WASM_STDIO_CONFIG = {
  args: argv,
  stdout: console.log,
};

const wasi = new WASI(CASSIE_WASM_STDIO_CONFIG);
const jsffiExports = {};

const wasmBuffer = readFileSync(WASM_REACTOR_PATH);
const importObject = Object.assign(
  { ghc_wasm_jsffi: ghc_wasm_jsffi(jsffiExports) },
  wasi.getImportObject()
);
const wasm = await WebAssembly.instantiate(wasmBuffer, importObject);

Object.assign(jsffiExports, wasm.instance.exports);
wasi.initialize(wasm);

await (wasi.instance.exports.mainJS as any as () => Promise<void>)();