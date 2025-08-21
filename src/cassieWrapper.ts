import { lstatSync, mkdirSync, PathLike, readFileSync, writeFileSync } from 'node:fs';
import { homedir } from 'node:os';
import { argv, chdir, cwd } from 'node:process';
import { WASI } from 'node:wasi';
import ghc_wasm_jsffi from './wasi/ghc_wasm_jsffi.js';

// ensures this value exists when experimental flag is passed to node (NodeJS >= 21.X.X)
const WASM_MODULE_PATH = `${import.meta.dirname}/wasi/reactor.wasm`;
const wasi = new WASI({ args: argv, version: "preview1", });
const jsffiExports = {};

Object.assign(globalThis, {
  fs_readFileSync: (path: string) => readFileSync(path).toString(),
  fs_writeFileSync: writeFileSync,
  fs_lstat_isFile: (path: PathLike) => lstatSync(path).isFile,
  fs_mkdir: (p: boolean, path: PathLike) => mkdirSync(path, { recursive: p }),
  os_homedir: homedir,
  process_cwd: cwd,
  process_chdir: chdir,
});

export async function wasiInit(): Promise<WebAssembly.Instance> {
  const wasmBuffer = readFileSync(WASM_MODULE_PATH);
  const importObject: WebAssembly.Imports = Object.assign(
    { ghc_wasm_jsffi: ghc_wasm_jsffi(jsffiExports), },
    wasi.getImportObject()
  );

  const { instance } = await WebAssembly.instantiate(wasmBuffer, importObject);
  Object.assign(jsffiExports, instance.exports);
  wasi.initialize(instance);

  return instance;
}
