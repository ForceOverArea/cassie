#!/bin/bash

CABAL_REACTOR_PATH=dist-newstyle/build/wasm32-wasi/ghc-9.12.2.20250327/cassie-cli-0.1.0.0/x/reactor/opt/build/reactor/reactor.wasm
DIST_REACTOR_DIR=dist/wasi
SRC_REACTOR_DIR=src/wasi

# Build wasm32-wasi reactor module using GHC/Cabal
wasm32-wasi-cabal build reactor

# Generate JSFFI source from reactor and create declaration .ts file
$(wasm32-wasi-ghc --print-libdir)/post-link.mjs -i $CABAL_REACTOR_PATH -o $SRC_REACTOR_DIR/ghc_wasm_jsffi.js
npx tsc $SRC_REACTOR_DIR/ghc_wasm_jsffi.js --allowJS --declaration --emitDeclarationOnly

# Compile Typescript to JS module (creates 'dist' directory)
npx tsc
cp $CABAL_REACTOR_PATH $DIST_REACTOR_DIR