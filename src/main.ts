import { wasiInit } from './cassieWrapper.js';

/**
 * Entry point for the NodeJS distribution of Cassie
 */
async function main() {
  const wasi = await wasiInit();
}

main().catch(reason => console.error(`ligma balls: ${reason}`));