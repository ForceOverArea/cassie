import { init } from './wasm/wasm_wrapper.js'

const SolnShowStates = {
  SYMBOLIC: "equation",
  NUMERIC: "maybeValue",
};

const EDITOR_ID = "editor-pane";
const SOLN_PANE_ID = "soln-pane";
const SYMBOLIC_TOGGLE_ID = "symbolic-toggle";
const NUMERIC_TOGGLE_ID = "numeric-toggle";

const EDITOR = document.getElementById(EDITOR_ID);
const SOLN_PANE = document.getElementById(SOLN_PANE_ID);
const SYMBOLIC_TOGGLE = document.getElementById(SYMBOLIC_TOGGLE_ID);
const NUMERIC_TOGGLE = document.getElementById(NUMERIC_TOGGLE_ID);

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

main();

async function trySolveSystem() { 
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

    for (const x of soln) {
      const solnTag = document.getElementById(solnUID(x.symbol));
      solnTag.onclick = () => { 
        const steps = showStepsFor(x.symbol, soln); //.split('');
        SOLN_PANE.innerHTML = steps;
      };
    }
  }
}

function showStepsFor(symbol, soln) {
  const target = soln.find(x => x.symbol == symbol);
  return `Showing steps to solve for ${symbol}:<br>${target.steps}`;
}

function renderSolnAs(soln, solnKind) {
  const classes = 'sym-soln-entry quick-fill quicker clickable';
  if (solnKind === SolnShowStates.NUMERIC) {
    return `${soln.symbol} = ${soln.maybeValue}`;
  } else if (solnKind === SolnShowStates.SYMBOLIC) {
    return `<div id="${solnUID(soln.symbol)}" class="${classes}">${soln.equation}</div>`;
  }
}

function solnUID(symbol) {
  return `cassie-soln-for-symbol-${symbol}`;
}