#!/bin/bash
set -e
WASM_REACTOR_PATH=./dist-newstyle/build/wasm32-wasi/ghc-9.12.2.20250327/cassie-wasm-0.1.0.0/x/cassie-wasm/build/cassie-wasm/cassie-wasm.wasm
PUBLIC_ASSET_DIR=./public/wasm

# build WASM module and move to page assets dir
echo 1/4 cleaning up...
cabal clean

echo 2/4 building WASM reactor...
wasm32-wasi-cabal build cassie-wasm >build_log.txt 2>build_log.txt

echo 3/4 copying WASM reactor to static page directory...
cp $WASM_REACTOR_PATH $PUBLIC_ASSET_DIR

# generate jsffi module
echo 4/4 generating jsffi module...
$(wasm32-wasi-ghc --print-libdir)/post-link.mjs -i $PUBLIC_ASSET_DIR/cassie-wasm.wasm -o $PUBLIC_ASSET_DIR/ghc_wasm_jsffi.js

echo done!