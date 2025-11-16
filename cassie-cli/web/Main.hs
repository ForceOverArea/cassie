module Main where
import CassieCLI.Solve
import GHC.Wasm.Prim

main :: IO ()
main = error "no main can be provided in a WASI reactor build due to GHC linker behavior" 

mainDOM :: JSString -> IO JSString
mainDOM source = pure . toJSString . show $ cassieWebMain . fromJSString $ source

foreign export javascript mainDOM :: JSString -> IO JSString