import { WASI } from 'https://cdn.jsdelivr.net/npm/@runno/wasi@0.7.0/dist/wasi.js';
import ghc_wasm_jsffi from './ghc_wasm_jsffi.js';

// the naming is bad here because we're dynamically fetching from our own page... yeah...
const WASM_MODULE_PATH = '/cassie/public/wasm/cassie-wasm.wasm';
const jsffiExports = {};
const wasi = new WASI({});

export async function init() {
  const wasm_methods = {};

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
  }

  wasm_methods.solveSystem = solveSystem;

  return wasm_methods;
}

const haskellStringCleanup = (x) => { 
  return x.replaceAll('\\\\n', '<br>')
    .replaceAll('\\"', '"')
    .slice(1, -1);
}

async function solveSystem(systemText) {
  let result;

  const sanitized = haskellStringCleanup(
    await wasi.instance.exports.solveSystemHs(systemText)
  );

  try {
    result = JSON.parse(sanitized);
    console.debug(JSON.stringify(result, null, 2));
  } catch (err) {
    result = sanitized;
    console.error(err);
    console.info(sanitized);
  }
  return result;
}