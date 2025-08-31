{-# LANGUAGE Trustworthy #-}
module JSBindings 
    ( evaluateJS
    , factorizeJS
    , isolateJS
    , substituteJS
    ) where

import GHC.Wasm.Prim
import safe JSHelper

evaluateJS :: JSString -> IO JSString
evaluateJS = pure . toJSString . evaluateStr . fromJSString

factorizeJS :: JSString -> JSString -> IO JSString
factorizeJS targetJS exprJS =
    let 
        target = fromJSString targetJS
        expr = fromJSString exprJS
    in pure . toJSString $ factorizeStr target expr

isolateJS :: JSString -> JSString -> IO JSString
isolateJS targetJS eqnJS =
    let 
        target = fromJSString targetJS
        eqn = fromJSString eqnJS
    in pure . toJSString $ isolateStr target eqn

substituteJS :: JSString -> JSString -> JSString -> IO JSString 
substituteJS targetJS replacementJS srcJS = 
    let 
        target = fromJSString targetJS
        replacement = fromJSString replacementJS
        src = fromJSString srcJS
    in pure . toJSString $ substituteStr target replacement src

foreign export javascript evaluateJS :: JSString -> IO JSString

foreign export javascript factorizeJS :: JSString -> JSString -> IO JSString

foreign export javascript isolateJS :: JSString -> JSString -> IO JSString

foreign export javascript substituteJS :: JSString -> JSString -> JSString -> IO JSString
