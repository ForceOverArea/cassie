module Main where

import Control.Arrow 
import Data.List
import qualified Data.Map as Map
import Data.Cassie (solveSystem, Solution)
import Data.Cassie.Internal 
import Data.Cassie.Structures (Symbol)
import GHC.Wasm.Prim

main :: IO ()
main = error "not necessary"

solveSystemHs :: JSString -> JSString
solveSystemHs systemJS = 
    let 
        system = fromJSString systemJS
    in case solveSystem system of
        Left  err  -> toJSString $ "{ error: \"" ++ show err ++ "\" }"
        Right soln 
            -> let 
                ks = Map.keys soln 
            in toJSString . show $ map (buildSolnObject soln) ks

buildSolnObject :: Solution -> Symbol -> String
buildSolnObject soln name = 
    let 
        (symbolic, steps, numeric) = postProcSoln $ soln Map.! name
        postProcSoln = first' show  
            >>> second' show 
            >>> third getNumValOrErr
        getNumValOrErr x = case x of
            Left y -> show y
            Right z -> show z
    in "{ symbol: \"" ++ name 
        ++ "\", equation: \"" ++ show symbolic 
        ++ "\", steps: \"" ++ show steps 
        ++ "\", value: \"" ++ show numeric 
        ++ "\", }"

foreign export javascript solveSystemHs :: JSString -> JSString
