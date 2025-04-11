import { WASI } from 'https://cdn.jsdelivr.net/npm/@runno/wasi@0.7.0/dist/wasi.js';
import ghc_wasm_jsffi from './ghc_wasm_jsffi.js';

// the naming is bad here because we're dynamically fetching from our own page... yeah...
const WASM_MODULE_PATH = '/cassie/public/wasm/cassie-wasm.wasm';
const jsffiExports = {};
const wasi = new WASI({});

async function ensure_initialized() {
  if (wasi.hasBeenInitialized) {
    return;
  } else {
    const wasm = await WebAssembly.instantiateStreaming(
      fetch(WASM_MODULE_PATH),
      Object.assign(
        { ghc_wasm_jsffi: ghc_wasm_jsffi(jsffiExports) },
        wasi.getImportObject()
      )
    );
    Object.assign(jsffiExports, wasm.instance.exports);
    wasi.initialize(wasm, {
      ghc_wasm_jsffi: ghc_wasm_jsffi(jsffiExports)
    });
    // TODO: can anything else be made lazy here?
  }
}

export async function solveFor(eqn, target) {
  await ensure_initialized();
  return wasi.instance.exports.solvedForHs(eqn, target);
}

export async function solveForValue(eqn, target, context) {
  const ctxItems = [];
  for (const [k, v] of Object.entries(context)) {
    ctxItems.push(`${k}=${v}`);
  }
  await ensure_initialized();
  return wasi.instance.exports.solvedForValueHs(eqn, target, ctxItems.join(','));
}