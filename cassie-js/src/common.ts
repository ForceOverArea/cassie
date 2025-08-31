import ghc_wasm_jsffi from './wasi/ghc_wasm_jsffi.js';
import { readFileSync } from 'node:fs';
import { argv } from 'node:process';
import { WASI } from 'node:wasi'; 

// Singletons/mutable data
const jsffiExports = {};
const wasi = new WASI({ 
  version: 'preview1', 
  args: argv,
});

// Constants
const WASM_REACTOR_PATH = `${import.meta.dirname}/wasi/reactor.wasm`;

/**
 * Runs the initialization steps for Cassie's WASI reactor module. 
 * 
 * @returns The functions exported from the WebAssembly module
 */
export async function initCassieWasiReactor(): Promise<WebAssembly.Exports> {
  const wasmBuffer = readFileSync(WASM_REACTOR_PATH);
  const importObject = Object.assign(
    { ghc_wasm_jsffi: ghc_wasm_jsffi(jsffiExports) },
    wasi.getImportObject()
  );    
  const { instance } = await WebAssembly.instantiate(wasmBuffer, importObject);
  Object.assign(jsffiExports, instance.exports);
  wasi.initialize(instance);

  return instance.exports;
}