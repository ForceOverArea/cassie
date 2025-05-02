import { SOLN_PANE } from "./domElements.js";

const SYM_SOLN_ID_PREFIX = 'sym-soln-';
const SYM_SOLN_CLASSLIST = [
  'sym-soln-entry ',
  'quick-fill',
  'clickable',
].join(' ');

/**
 * 
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
    + '<br>&nbsp;<br>Solution:<br>'
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
 * 
 * @param {Object} cassieSoln 
 */
export function renderNumeric(cassieSoln) {
  const numSolns = [];

  for (const { symbol, maybeValue } of cassieSoln.solution) {
    numSolns.push(`${symbol} = ${maybeValue}`);
  }

  SOLN_PANE.innerHTML = renderContext(cassieSoln.context)
    + '<br>&nbsp;<br>Solution:<br>'
    + numSolns.join('<br>');
} 

/**
 * 
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

  return `Context:<br>${funcText}<br><br>${cnstText}<br>`;
}

function wrapWithSolutionTag(solutionText, uid) {
  return `<div id="${SYM_SOLN_ID_PREFIX + uid}" class="${SYM_SOLN_CLASSLIST}">${solutionText}</div>`;
}

// const solution = soln.solution;
//     console.log(soln.context);
//     const systemSoln = [];
  
//     for (const solved of solution) {
//       systemSoln.push(
//         renderSolnAs(solved, solnShowState_global)
//       );
//     }
  
//     SOLN_PANE.innerHTML = systemSoln.join('<br>');

//     for (const x of solution) {
//       const solnTag = document.getElementById(solnUID(x.symbol));
//       solnTag.onclick = () => { 
//         const steps = showStepsFor(x.symbol, solution); //.split('');
//         SOLN_PANE.innerHTML = steps;
//       };
//     }

// function showStepsFor(symbol, soln) {
//     const target = soln.find(x => x.symbol == symbol);
//     return `Showing steps to solve for ${symbol}:<br>${target.steps}`;
//   }
  
//   function renderSolnAs(soln, solnKind) {
//     const classes = 'sym-soln-entry quick-fill quicker clickable';
//     if (solnKind === SolnShowStates.NUMERIC) {
//       return `${soln.symbol} = ${soln.maybeValue}`;
//     } else if (solnKind === SolnShowStates.SYMBOLIC) {
//       return `<div id="${solnUID(soln.symbol)}" class="${classes}">${soln.equation}</div>`;
//     }
//   }
  
//   function solnUID(symbol) {
//     return `cassie-soln-for-symbol-${symbol}`;
//   }