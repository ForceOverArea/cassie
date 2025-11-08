module Main where
import GHC.Wasm.Prim

main = error "no main can be provided in a WASI reactor build due to GHC linker behavior" 

mainDOM :: JSString -> IO JSString
mainDOM source = 

foreign export javascript mainDOM :: JSString -> IO JSString