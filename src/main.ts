import { readFileSync, writeFileSync, lstatSync, mkdirSync, PathLike } from 'node:fs';
import { homedir } from 'node:os';
import { argv, chdir, cwd, env, stdin, stdout } from 'node:process';
import { createInterface } from 'node:readline';
import { WASI, WASIContextOptions } from '@runno/wasi'; // use runno since Node's WASI implementation gets indigestion from Haskell's WASM
import ghc_wasm_jsffi from './wasi/ghc_wasm_jsffi.js';
import { Blob } from 'node:buffer';

const UTF8 = 'utf-8';

// Make interface functions available for Haskell
globalThis.fs_readFileSync = (path: PathLike) => readFileSync(path, { encoding: UTF8 }).toString();
globalThis.fs_writeFileSync = (path: PathLike, contents: string) => writeFileSync(path, contents, { encoding: UTF8 });
globalThis.fs_lstat_isFile = (path: PathLike) => lstatSync(path).isFile();
globalThis.fs_mkdir = (rcsv: boolean, path: PathLike) => mkdirSync(path, { recursive: Boolean(rcsv) });
globalThis.os_homedir = homedir;
globalThis.process_cwd = cwd;
globalThis.process_chdir = chdir;

const WASM_REACTOR_PATH = `${import.meta.dirname}/wasi/reactor.wasm`;
const CASSIE_STDIN_IF = createInterface(stdin, stdout);

function cassieReadline(byteLimit: number) {
  let input = '';
  CASSIE_STDIN_IF.question('', x => {
    const inputBuffer = Buffer.from(x);
    input = Uint8Array.prototype.slice
      .call(inputBuffer, [0, byteLimit])
      .toString();
  });
  return input;
};

const CASSIE_WASM_STDIO_CONFIG: Partial<WASIContextOptions> = {
  args: argv,
  // env,
  // stderr: console.error,
  stdin: cassieReadline,
  stdout: console.log,
  isTTY: true
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