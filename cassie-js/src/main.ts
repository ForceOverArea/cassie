#!/usr/bin/env node
import { readFileSync, writeFileSync, lstatSync, mkdirSync, PathLike } from 'node:fs';
import { homedir } from 'node:os';
import { argv, chdir, cwd, stdin, stdout } from 'node:process';
import { createInterface } from 'node:readline/promises';
import { WASI } from 'node:wasi';
import { initCassieWasiReactor } from './common.js';

// Constants 
const CASSIE_STDIN_IF = createInterface({ input: stdin, output: stdout });

// Make interface functions available for Haskell
globalThis.fs_readFileSync = (path: PathLike) => readFileSync(path, { encoding: 'utf-8' }).toString();
globalThis.fs_writeFileSync = (path: PathLike, contents: string) => writeFileSync(path, contents, { encoding: 'utf-8' });
globalThis.fs_lstatSync_isFile = (path: PathLike) => lstatSync(path).isFile();
globalThis.fs_mkdirSync = (rcsv: boolean, path: PathLike) => mkdirSync(path, { recursive: Boolean(rcsv) });
globalThis.os_homedir = homedir;
globalThis.process_cwd = cwd;
globalThis.process_chdir = chdir;
globalThis.readline_Interface_question = () => CASSIE_STDIN_IF.question('>>> ');
globalThis.console_log = console.log;

/**
 * Entry point for main script
 */
async function main() {
  let caughtErr = undefined;
  try {
    const exports = await initCassieWasiReactor();
    if (exports.mainJS instanceof Function) {
      await exports.mainJS();
    } else {
      throw new TypeError('webassembly export "mainJS" must be of type "Function"');
    }
  } catch (err) {
    caughtErr = err;
  } finally {
    // START cleanup
    CASSIE_STDIN_IF.close();
    // END cleanup
    if ('undefined' !== typeof caughtErr) {
      throw caughtErr;
    }
  }
}

await main();
