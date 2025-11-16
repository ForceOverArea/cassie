import { SOLN_PANE } from './domElements.js';

const SYM_SOLN_ID_PREFIX = 'sym-soln-';
const SYM_SOLN_CLASSLIST = [
  'sym-soln-entry',
  'quick-fill',
  'clickable',
].join(' ');

/**
 * @param {Array} cassieSoln 
 */
export function renderSymbolic(cassieSoln) {
  const [ctx, foundValues] = cassieSoln; // solution consists of "tuple" of objects
  const symSolns = [];

  console.log(JSON.stringify(cassieSoln.solution));

  // Generate list of equations to add to HTML
  try {
    for (const [_, soln] of Object.entries(foundValues)) {
      symSolns.push(wrapWithSolutionTag(soln.constrained, symSolns.length));
    }
  } catch (err) {
    console.log(JSON.stringify(symSolns));
    throw err;
  }

  // Add eqns to HTML
  SOLN_PANE.innerHTML = renderContext(ctx) 
    + `<br>&nbsp;<br>${solnHeader('Solution:')}<br>`
    + symSolns.join('<br>');

  // Add onclick events to new tags
  for (const [i, [symbol, { steps }]] of Object.entries(Object.entries(foundValues))) {
    const elem = document.getElementById(SYM_SOLN_ID_PREFIX + i);
    elem.onclick = () => {
      SOLN_PANE.innerHTML = solnHeader(`Showing steps to solve for ${symbol}:`) 
        + `<br>${steps.join('<br>&nbsp;<br>')}`;
    }
  }
}

/**
 * @param {Object} cassieSoln 
 */
export function renderNumeric(cassieSoln) {
  const [ctx, foundValues] = cassieSoln; // solution consists of "tuple" of objects
  const numSolns = [];

  for (const [symbol, {possVal}] of Object.entries(foundValues)) {
    numSolns.push(`${symbol} = ${possVal}`);
  }

  SOLN_PANE.innerHTML = renderContext(ctx)
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
