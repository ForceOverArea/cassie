import { initCassieWasiReactor, CASSIE_ISSUES_URL } from './common.js';

export type Equation = string;
export type Expression = string;
export type Symbol = string;

let InitializedWASI: WebAssembly.Exports | undefined;

class CassieWASIImportError extends Error {
  constructor(missingImport: string) {
    super(`could not find function '${missingImport}' imported from cassie. please report this issue at ${CASSIE_ISSUES_URL}`);
  }
}

async function ensureInitialized(): Promise<void> {
  if ('undefined' === typeof InitializedWASI) {
    InitializedWASI = await initCassieWasiReactor();
  }
}

export async function isolate(
  equation: Equation, 
  target: Symbol
): Promise<Equation> {
  await ensureInitialized();
  if (InitializedWASI.isolateJS instanceof Function) {
    return await InitializedWASI.isolateJS(target, equation);
  } else {
    throw new CassieWASIImportError('isolateJS');
  }
} 

export async function evaluate(
  expression: Expression
): Promise<number> {
  await ensureInitialized();
  if (InitializedWASI.evaluateJS instanceof Function) {
    const result: string = await InitializedWASI.evaluateJS(expression);
    try {
      return Number.parseFloat(result);
    } catch (err) {
      throw new Error(result);
    }
  } else {
    throw new CassieWASIImportError('evaluateJS');
  }
}

export async function substitute(
  expression: Expression, 
  find: Symbol, 
  replace: Expression
): Promise<Expression> {
  await ensureInitialized();
  if (InitializedWASI.substituteJS instanceof Function) {
    return await InitializedWASI.substituteJS(find, replace, expression);
  } else {
    throw new CassieWASIImportError('substituteJS');
  }
}

export async function factorize(
  expression: Expression, 
  target: Symbol
): Promise<Expression> {
  await ensureInitialized();
  if (InitializedWASI.factorizeJS instanceof Function) {
    return await InitializedWASI.factorizeJS(expression, target);
  } else {
    throw new CassieWASIImportError('factorizeJS');
  }
}