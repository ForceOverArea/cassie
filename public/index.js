import { init } from './wasm/wasm_wrapper.js'

const SolnShowStates = {
  SYMBOLIC: "equation",
  NUMERIC: "maybeValue",
};

const EDITOR_ID = "editor-pane";
const SOLN_PANE_ID = "soln-pane";
const SYMBOLIC_TOGGLE_ID = "symbolic-toggle";
const NUMERIC_TOGGLE_ID = "numeric-toggle";

let solnShowState_global = SolnShowStates.SYMBOLIC;
const wasmImports = {
  solveSystem() {}
};

init().then((wasi_reactor) => {
  Object.assign(wasmImports, wasi_reactor);
});

// script is deferred, so main function can safely access DOM.
function main() {
  const EDITOR = document.getElementById(EDITOR_ID);
  const SOLN_PANE = document.getElementById(SOLN_PANE_ID);
  const SYMBOLIC_TOGGLE = document.getElementById(SYMBOLIC_TOGGLE_ID);
  const NUMERIC_TOGGLE = document.getElementById(NUMERIC_TOGGLE_ID);
  
  EDITOR.oninput = async () => { 
    const soln = await wasmImports.solveSystem(EDITOR.innerText);
    
    if ('string' === typeof soln) {
      SOLN_PANE.innerHTML = soln.toString();
    
    } else { // assuming solution is an array of objects
      const systemSoln = [];
      
      for (const solved of soln) {
        systemSoln.push(
          renderSolnAs(solved, solnShowState_global)
        );
      }
      
      SOLN_PANE.innerHTML = systemSoln.join('<br>');
    }
  }

  SYMBOLIC_TOGGLE.onclick = () => {
    solnShowState_global = SolnShowStates.SYMBOLIC;
    EDITOR.oninput();
  }

  NUMERIC_TOGGLE.onclick = () => {
    solnShowState_global = SolnShowStates.NUMERIC;
    EDITOR.oninput();
  }
}

main();

function renderSolnAs(soln, solnKind) {
  if (solnKind === SolnShowStates.NUMERIC) {
    return `${soln.symbol} = ${soln.maybeValue}`;
  } else if (solnKind === SolnShowStates.SYMBOLIC) {
    return `${soln.equation}`;
  }
}