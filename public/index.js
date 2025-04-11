import { solveFor } from './wasm/wasm_wrapper.js'

const EDITOR_ID = "editor-pane";
const SOLN_PANE_ID = "soln-pane";
const IDENTIFIER_REGEX = /[a-z_][a-z0-9_]*/ig;

async function solveEquations(sys) {
  const idents = sys.matchAll(IDENTIFIER_REGEX);
  const results = [];
  let soln = "";
  
  for (const i of idents) {
    soln = await solveFor(sys, i);
    console.info(soln);
    results.push(soln);
  }

  return results;
}

// script is deferred, so main function can safely access DOM.
function main() {
  const editor = document.getElementById(EDITOR_ID);
  const soln_pane = document.getElementById(SOLN_PANE_ID);
  
  editor.oninput = async () => { 
    const solns = await solveEquations(editor.innerText);
    soln_pane.innerHTML = solns.join("<br>"); 
  }
}

main();