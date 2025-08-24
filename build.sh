#!/bin/bash
set -e
CABAL_REACTOR_PATH=dist-newstyle/build/wasm32-wasi/ghc-9.12.2.20250327/cassie-cli-0.1.0.0/x/reactor/opt/build/reactor/reactor.wasm
DIST_REACTOR_DIR=dist/wasi
SRC_REACTOR_DIR=src/wasi

# Get JS + WASM needed for NPM package
echo "[1/6] Building Haskell project..."
wasm32-wasi-cabal build reactor >> build_log.txt

echo "[2/6] 'npm install'-ing..."
npm install >> build_log.txt

# Generate JSFFI source from reactor and create declaration .ts file
echo "[3/6] Generating FFI bindings..."
$(wasm32-wasi-ghc --print-libdir)/post-link.mjs -i $CABAL_REACTOR_PATH -o $SRC_REACTOR_DIR/ghc_wasm_jsffi.js >> build_log.txt

echo "[4/6] Generating FFI TypeScript declaration..."
npx tsc $SRC_REACTOR_DIR/ghc_wasm_jsffi.js --allowJS --declaration --emitDeclarationOnly >> build_log.txt

# Create  JavaScript build in 'dist' directory
echo "[5/6] Compiling TypeScript..."
npx tsc >> build_log.txt

echo "[6/6] Copying Haskell reactor module to 'dist' directory..."
cp $CABAL_REACTOR_PATH $DIST_REACTOR_DIR >> build_log.txt

echo "Done!"