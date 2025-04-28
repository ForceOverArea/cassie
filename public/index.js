import { solveSystem } from './wasm/wasm_wrapper.js'

const EDITOR_ID = "editor-pane";
const SOLN_PANE_ID = "soln-pane";
// const IDENTIFIER_REGEX = /[a-z_][a-z0-9_]*/ig;

// script is deferred, so main function can safely access DOM.
function main() {
  const editor = document.getElementById(EDITOR_ID);
  const soln_pane = document.getElementById(SOLN_PANE_ID);
  
  editor.oninput = async () => { 
    const soln = await solveSystem(editor.innerText);
    if ('string' === typeof soln) {
      soln_pane.innerHTML = soln.toString();
    } else { // assuming solution is an array of objects
      const systemSoln = [];
      for (const solved of soln) {
        systemSoln.push(solved.equation);
      }
      soln_pane.innerHTML = systemSoln.join('<br>');
    }
  }
}

main();