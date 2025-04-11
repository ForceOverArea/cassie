#!/bin/bash
WASM_REACTOR_PATH=./dist-newstyle/build/wasm32-wasi/ghc-9.12.2.20250327/cassie-wasm-0.1.0.0/x/cassie-wasm/build/cassie-wasm/cassie-wasm.wasm
PUBLIC_ASSET_DIR=./public/wasm

set -e
# build WASM module and move to page assets dir
wasm32-wasi-cabal build cassie-wasm
mv $WASM_REACTOR_PATH $PUBLIC_ASSET_DIR

# generate jsffi module
$(wasm32-wasi-ghc --print-libdir)/post-link.mjs -i $PUBLIC_ASSET_DIR/cassie-wasm.wasm -o $PUBLIC_ASSET_DIR/ghc_wasm_jsffi.js