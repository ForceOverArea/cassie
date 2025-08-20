import { wasiInit } from './cassieWrapper.js';

/**
 * Entry point for the NodeJS distribution of Cassie
 */
async function main() {
  const wasi = await wasiInit();
  await wasi
    .instance
    .exports
    .mainJS();
}

main();