import { SOLN_PANE } from './domElements.js';

const SYM_SOLN_ID_PREFIX = 'sym-soln-';
const SYM_SOLN_CLASSLIST = [
  'sym-soln-entry ',
  'quick-fill',
  'clickable',
].join(' ');

/**
 * @param {Array} cassieSoln 
 */
export function renderSymbolic(cassieSoln) {
  const symSolns = [];

  // Generate list of equations to add to HTML
  for (const { equation } of cassieSoln.solution) {
    symSolns.push(wrapWithSolutionTag(equation, symSolns.length));
  }

  // Add eqns to HTML
  SOLN_PANE.innerHTML = renderContext(cassieSoln.context) 
    + `<br>&nbsp;<br>${solnHeader('Solution:')}<br>`
    + symSolns.join('<br>');

  // Add onclick events to new tags
  for (const [i, { steps, symbol }] of cassieSoln.solution.entries()) {
    const elem = document.getElementById(SYM_SOLN_ID_PREFIX + i);
    elem.onclick = () => {
      SOLN_PANE.innerHTML = `Showing steps to solve for ${symbol}:<br>${steps}`;
    }
  }
}

/**
 * @param {Object} cassieSoln 
 */
export function renderNumeric(cassieSoln) {
  const numSolns = [];

  for (const { symbol, maybeValue } of cassieSoln.solution) {
    numSolns.push(`${symbol} = ${maybeValue}`);
  }

  SOLN_PANE.innerHTML = renderContext(cassieSoln.context)
    + `<br>&nbsp;<br>${solnHeader('Solution:')}<br>`
    + numSolns.join('<br>');
} 

/**
 * @param {Object} cassieCtx 
 */
function renderContext(cassieCtx) {
  const constants = [];
  const functions = [];
  
  for (const [symbol, value] of Object.entries(cassieCtx)) {
    if (value.includes('-&gt;')) {
      functions.push(`${symbol}${value}`);
    } else {
      constants.push(`${symbol} = ${value}`);
    }
  }

  const funcText = functions.join('<br>');
  const cnstText = constants.join('<br>');

  return `${solnHeader('Context:')}<br>${funcText}<br><br>${cnstText}<br>`;
}

function wrapWithSolutionTag(solutionText, uid) {
  return `<div id="${SYM_SOLN_ID_PREFIX + uid}" class="${SYM_SOLN_CLASSLIST}">${solutionText}</div>`;
}

function solnHeader(str) {
  return `<div class="soln-header">${str}</div>`
}
