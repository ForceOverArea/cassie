module Main where

import Control.Arrow ((|||))
import Data.Cassie (solvedFor, solvedForValue')
import Data.Cassie.Internal (parseContextString)
import Data.Cassie.Structures (Equation)
import GHC.Wasm.Prim

main = error "not necessary"

solvedForHs :: JSString -> JSString -> JSString
solvedForHs eqn target = 
    let eqn' = fromJSString eqn
        target' = fromJSString target
    in case eqn' `solvedFor` target' of
        Left _ -> toJSString ""
        Right (soln, _steps) -> toJSString $ show soln

solvedForValueHs :: JSString -> JSString -> JSString -> Double
solvedForValueHs eqn target ctx =
    let ctx' = parseContextString $ fromJSString ctx
        eqn' = fromJSString eqn
        target' = fromJSString target
    in case solvedForValue' eqn' target' ctx' of
        Left _ -> nan
        Right (soln, _, _) -> soln
    where 
        nan :: Double
        nan = 0.0 / 0.0

foreign export javascript solvedForHs :: JSString -> JSString -> JSString
foreign export javascript solvedForValueHs :: JSString -> JSString -> JSString -> Double