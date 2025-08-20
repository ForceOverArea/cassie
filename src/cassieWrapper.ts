import { lstatSync, mkdirSync, PathLike, readFileSync, writeFileSync } from 'node:fs';
import { homedir } from 'node:os';
import { argv, chdir, cwd } from 'node:process';
import { WASI } from 'node:wasi';
import ghc_wasm_jsffi from './wasi/ghc_wasm_jsffi.js';

// ensures this value exists when experimental flag is passed to node (NodeJS >= 21.X.X)
const __dirname = import.meta.dirname;

// Filesystem API courtesy of NodeJS. 
// This is injected into the WASI reactor to give Haskell Filesystem access.
const NodeFileSystemIF = {
  fs_readFileSync : readFileSync,
  fs_writeFileSync: writeFileSync,
  fs_lstat_isFile : (path: PathLike) => lstatSync(path).isFile,
  fs_mkdir        : (p: boolean, path: PathLike) => mkdirSync(path, { recursive: p }),
  os_homedir      : homedir,
  process_cwd     : cwd,
  process_chdir   : chdir,
};

const WASM_MODULE_PATH = `${__dirname}/wasi/reactor.wasm`;
const jsffiExports = {};
const wasi = new WASI({ 
  args: argv,
  version: "preview1", 
});

export interface Wasm32WasiCassieModule {
  instance: { exports: { mainJS: () => Promise<void> } }
};

export async function wasiInit(): Promise<Wasm32WasiCassieModule> {
  await wasiInit_initialize();
  return (wasi as Object) as Wasm32WasiCassieModule;
}

async function wasiInit_initialize(): Promise<void> {
  const wasmBuffer = readFileSync(WASM_MODULE_PATH);
  const importObject: WebAssembly.Imports = Object.assign(
    { ghc_wasm_jsffi: ghc_wasm_jsffi(jsffiExports), },
    wasi.getImportObject()
  );

  const { instance } = await WebAssembly.instantiate(wasmBuffer, importObject);
  Object.assign(jsffiExports, instance.exports);
  wasi.initialize(instance);
}