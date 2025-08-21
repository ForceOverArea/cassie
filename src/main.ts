import { wasiInit } from './cassieWrapper.js';
await ((await wasiInit()).exports.mainJS as any as () => Promise<void>)();