import { solveFor, solveForValue } from './wasm/wasm_wrapper.js'

const EQUATION = "A + B * C = D";
const CTX = { A: 1, B: 2, D: 4 };

// script is deferred, so main function is safe to access DOM.
function main() {
  console.log("ligma");
  let elem = document.getElementById("soln-pane");
  console.log(elem);
  elem.innerHTML = "hello!";
}

main();