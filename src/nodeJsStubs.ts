import { lstatSync, mkdirSync, PathLike, readFileSync, writeFileSync } from 'node:fs';
import { homedir } from 'node:os';
import { chdir, cwd } from 'process';

export const fs_readFileSync    = readFileSync;
export const fs_writeFileSync   = writeFileSync;
export const fs_lstat_isFile    = (path:PathLike) => lstatSync(path).isFile;
export const fs_mkdir           = (p:boolean, path:PathLike) => mkdirSync(path, { recursive: p });
export const os_homedir         = homedir;
export const process_cwd        = cwd;
export const process_chdir      = chdir;