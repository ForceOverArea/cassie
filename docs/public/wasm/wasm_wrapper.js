import { WASI } from 'https://cdn.jsdelivr.net/npm/@runno/wasi@0.7.0/dist/wasi.js';
import ghc_wasm_jsffi from './ghc_wasm_jsffi.js';

// the naming is bad here because we're dynamically fetching from our own page... yeah...
const WASM_MODULE_PATH = '/cassie/public/wasm/web.wasm';
const jsffiExports = {};
const wasi = new WASI({});

export async function init() {
  const wasm_methods = {};

  if (wasi.hasBeenInitialized) {
    return;
  } else {
    let fetchedWasm = await fetch(WASM_MODULE_PATH);
    
    // This is a hack to make this script work with the deno dev server script.
    if (fetchedWasm.status !== 200) {
      console.warn('Falling back on alt path for deno server. You should not see this warning in prod.');
      fetchedWasm = await fetch(
        WASM_MODULE_PATH.replace('/cassie', '')
      );
    }

    const wasm = await WebAssembly.instantiateStreaming(
      fetchedWasm,
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
  return x
    .replaceAll('<', '&lt;')
    .replaceAll('>', '&gt;')
    .replaceAll('\\\\n', '<br>')
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