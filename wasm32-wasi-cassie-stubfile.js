import { lstat, mkdir, readFileSync, writeFileSync } from 'node:fs';
import { homedir } from 'node:os';
import { chdir, cwd } from 'node:process';

export const fs_readFileSync    = readFileSync;
export const fs_writeFileSync   = writeFileSync;
export const fs_lstat_isFile    = lstat.isFile;
export const fs_mkdir           = (p, path) => mkdir(path, { recursive: p });
export const os_homedir         = homedir;
export const process_cwd        = cwd;
export const process_chdir      = chdir;