import { init } from './wasm/wasm_wrapper.js';
import { renderNumeric, renderSymbolic } from './outputRendering.js';
import { EDITOR, NUMERIC_TOGGLE, SOLN_PANE, SYMBOLIC_TOGGLE } from "./domElements.js";

const SolnShowStates = {
  SYMBOLIC: "equation",
  NUMERIC: "maybeValue",
};

let solnShowState_global = SolnShowStates.SYMBOLIC;
const wasmImports = {
  solveSystem() {}
};

init().then((wasi_reactor) => {
  Object.assign(wasmImports, wasi_reactor);
});

// script is deferred, so main function can safely access DOM.
function main() {
  EDITOR.oninput = trySolveSystem;

  SYMBOLIC_TOGGLE.onclick = () => {
    solnShowState_global = SolnShowStates.SYMBOLIC;
    EDITOR.oninput();
  }

  NUMERIC_TOGGLE.onclick = () => {
    solnShowState_global = SolnShowStates.NUMERIC;
    EDITOR.oninput();
  }
}

async function trySolveSystem() { 
  const soln = await wasmImports.solveSystem(EDITOR.innerText);
  
  if ('string' === typeof soln) {
    SOLN_PANE.innerHTML = soln;
  } else if (SolnShowStates.SYMBOLIC === solnShowState_global) {
    renderSymbolic(soln);
  } else {
    renderNumeric(soln);
  }
}

main();
