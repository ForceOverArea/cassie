import { lstatSync, mkdirSync, PathLike, readFileSync, writeFileSync } from 'node:fs';
import { homedir } from 'node:os';
import { chdir, cwd } from 'node:process';
import { WASI } from 'node:wasi';
import ghc_wasm_jsffi from './wasi/ghc_wasm_jsffi.js';

// Filesystem API courtesy of NodeJS. 
// This is injected into the WASI reactor to give Haskell Filesystem access.
export const fs_readFileSync    = readFileSync;
export const fs_writeFileSync   = writeFileSync;
export const fs_lstat_isFile    = (path: PathLike) => lstatSync(path).isFile;
export const fs_mkdir           = (p: boolean, path: PathLike) => mkdirSync(path, { recursive: p });
export const os_homedir         = homedir;
export const process_cwd        = cwd;
export const process_chdir      = chdir;

const WASM_MODULE_PATH = './reactor.wasm';
const wasi = new WASI({ version: "preview1" });
const jsffiExports = {};

interface Wasm32WasiCassieModule {
  instance: { exports: { mainJS: () => Promise<void> } }
};

export async function wasiInit(): Promise<void> {
  const wasm = await WebAssembly.instantiateStreaming(
    fetch(new URL(WASM_MODULE_PATH)),
    Object.assign(
      { ghc_wasm_jsffi: ghc_wasm_jsffi(jsffiExports) },
      wasi.getImportObject()
    )
  );
  Object.assign(jsffiExports, wasm.instance.exports);
  wasi.initialize(wasm);
}

export async function cassieMain(): Promise<void> {
  const wasi_ = (wasi as Object) as Wasm32WasiCassieModule;
  await wasi_.instance.exports.mainJS();
}