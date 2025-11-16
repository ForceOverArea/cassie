import { init } from './wasm/wasm_wrapper.js';
import { renderNumeric, renderSymbolic } from './outputRendering.js';
import { EDITOR, NUMERIC_TOGGLE, SOLN_PANE, SYMBOLIC_TOGGLE } from "./domElements.js";

const SolnShowStates = {
  SYMBOLIC: "equation",
  NUMERIC: "maybeValue",
};

let solnShowState_global = SolnShowStates.SYMBOLIC;
const wasmImports = {
  /**
   * @param {string} _ 
   * @returns {object}
   */
  solveSystem(_) {}
};

function onInitialization() {
  trySolveSystem();
}

// script is deferred, so main function can safely access DOM.
function main() {
  EDITOR.oninput = trySolveSystem;

  SYMBOLIC_TOGGLE.onclick = () => {
    solnShowState_global = SolnShowStates.SYMBOLIC;
    trySolveSystem();
  }

  NUMERIC_TOGGLE.onclick = () => {
    solnShowState_global = SolnShowStates.NUMERIC;
    trySolveSystem();
  }
}

async function trySolveSystem() { 
  const soln = await wasmImports.solveSystem(EDITOR.innerText);
  
  if (typeof soln === 'string') { // just print out errors. TODO: fix 'show' implementations for error types
    SOLN_PANE.innerHTML = soln;
  } else if (SolnShowStates.SYMBOLIC === solnShowState_global) {
    renderSymbolic(soln);
  } else {
    renderNumeric(soln);
  }
}

main();

init().then((wasi_reactor) => {
  Object.assign(wasmImports, wasi_reactor);
  onInitialization();
});
