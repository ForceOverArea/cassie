#!/bin/bash
set -e
CABAL_REACTOR_PATH=dist-newstyle/build/wasm32-wasi/ghc-9.12.2.20250327/cassie-cli-0.1.0.0/x/reactor/opt/build/reactor/reactor.wasm
DIST_REACTOR_DIR=dist/wasi
CABAL_WEB_PATH=dist-newstyle/build/wasm32-wasi/ghc-9.12.2.20250327/cassie-cli-0.1.0.0/x/web/opt/build/web/web.wasm
DIST_WEB_DIR=docs/public/wasm
SRC_REACTOR_DIR=src/wasi
NUM_STEPS=9

# Get JS + WASM needed for NPM package
echo "[1/$NUM_STEPS] Building Haskell project for NodeJS..."
wasm32-wasi-cabal build reactor

echo "[2/$NUM_STEPS] 'npm install'-ing..."
npm install

# Generate JSFFI source from reactor and create declaration .ts file
echo "[3/$NUM_STEPS] Generating FFI bindings..."
mkdir -p src/wasi
$(wasm32-wasi-ghc --print-libdir)/post-link.mjs -i $CABAL_REACTOR_PATH -o $SRC_REACTOR_DIR/ghc_wasm_jsffi.js

echo "[4/$NUM_STEPS] Generating FFI TypeScript declaration..."
npx tsc $SRC_REACTOR_DIR/ghc_wasm_jsffi.js --allowJS --declaration --emitDeclarationOnly

# Create  JavaScript build in 'dist' directory
echo "[5/$NUM_STEPS] Compiling TypeScript..."
npx tsc

echo "[6/$NUM_STEPS] Copying Haskell reactor module to 'dist' directory..."
cp $CABAL_REACTOR_PATH $DIST_REACTOR_DIR

echo "[7/$NUM_STEPS] Building Haskell project for web..."
wasm32-wasi-cabal build web

echo "[8/$NUM_STEPS] Generating web FFI bindings..."
$(wasm32-wasi-ghc --print-libdir)/post-link.mjs -i $CABAL_WEB_PATH -o $DIST_WEB_DIR/ghc_wasm_jsffi.js

echo "[9/$NUM_STEPS] Copying Haskell reactor module for web to 'docs' directory..."
cp $CABAL_WEB_PATH $DIST_WEB_DIR