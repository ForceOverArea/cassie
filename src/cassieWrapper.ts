import { lstatSync, mkdirSync, PathLike, readFileSync, writeFileSync } from 'node:fs';
import { homedir } from 'node:os';
import { chdir, cwd } from 'node:process';
import { WASI } from 'node:wasi';
import ghc_wasm_jsffi from './wasi/ghc_wasm_jsffi.js';

// ensures this value exists when experimental flag is passed to node (NodeJS >= 21.X.X)
const __dirname = import.meta.dirname;

// Filesystem API courtesy of NodeJS. 
// This is injected into the WASI reactor to give Haskell Filesystem access.
const fs_readFileSync    = readFileSync;
const fs_writeFileSync   = writeFileSync;
const fs_lstat_isFile    = (path: PathLike) => lstatSync(path).isFile;
const fs_mkdir           = (p: boolean, path: PathLike) => mkdirSync(path, { recursive: p });
const os_homedir         = homedir;
const process_cwd        = cwd;
const process_chdir      = chdir;

const WASM_MODULE_PATH = `${__dirname}/wasi/reactor.wasm`;
const wasi = new WASI({ version: "preview1" });
const jsffiExports = {};

export interface Wasm32WasiCassieModule {
  instance: { exports: { mainJS: () => Promise<void> } }
};

export async function wasiInit(): Promise<Wasm32WasiCassieModule> {
  await wasiInit_initialize();
  return (wasi as Object) as Wasm32WasiCassieModule;
}

async function wasiInit_initialize(): Promise<void> {
  const wasmBuffer = readFileSync(WASM_MODULE_PATH);
  const importObject = Object.assign(
    { ghc_wasm_jsffi: ghc_wasm_jsffi(jsffiExports) }, 
    wasi.getImportObject()
  );

  const { instance } = await WebAssembly.instantiate(wasmBuffer, importObject);
  Object.assign(jsffiExports, instance.exports);
  wasi.initialize(instance);
}