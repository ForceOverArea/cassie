import * as fs      from 'node:fs';
import * as os      from 'node:os';
import * as process from 'node:process';

export const fs_readFileSync    = fs.readFileSync;
export const fs_writeFileSync   = fs.writeFileSync;
export const fs_lstat_isFile    = fs.lstat.isFile;
export const os_homedir         = os.homedir;
export const process_cwd        = process.cwd;
export const process_chdir      = process.chdir;