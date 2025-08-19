import { cassieMain, wasiInit } from "./cassieWrapper.js";

async function main() {
    await wasiInit();
    await cassieMain();
}

main();