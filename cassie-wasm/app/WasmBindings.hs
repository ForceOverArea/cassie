module Main where

import Control.Arrow 
import Control.Monad
import qualified Data.Map as Map
import Data.Aeson ()
import Data.Cassie (solveSystem, CassieError, Solution)
import Data.Cassie.Internal 
import Data.Cassie.Isolate (Steps)
import Data.Cassie.Structures (Equation, Symbol)
import GHC.Wasm.Prim

main = error "not necessary"

solveSystemHs :: JSString -> JSVal -> JSVal
solveSystemHs systemJS = 
    let 
        system = fromJSString systemJS
    in case solveSystem system of
        Left  err  -> (toJSString . show) err
        Right soln -> let ks = Map.keys soln 
            in map (buildSolnObject soln) ks

buildSolnObject :: Solution -> Symbol -> JSVal
buildSolnObject soln name = 
    let 
        f = first' getJSStrRep
            >>> second' buildStepsArray
            >>> third buildNumSolns

        (symbolic, steps, numeric) = f $ soln Map.! name
    in toJSArray [toJSString name, symbolic, steps, numeric]

getJSStrRep :: Show a => a -> JSString
getJSStrRep = toJSString . show

buildStepsArray :: Steps -> JSVal
buildStepsArray steps = toJSArray $ map toJSString steps

buildNumSolns :: Either CassieError Double -> JSString
buildNumSolns = join (|||) getJSStrRep

foreign export javascript solveSystemHs :: JSString -> JSVal
