{-# LANGUAGE Safe #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Cassie.Structures.UnarySystems 
    ( TrigUnary(..)
    , CancelUnary(..)
    , UnaryMock(..)
    ) where

import Data.Cassie.Structures.Internal

class UnaryMock u n where
    -- | Maps a marker representing a unary system operation to its appropriate
    --   unary operation.
    evalUnary :: u -> (n -> n)

-- | Typeclass for values that represent a set of binary operations that form 
--   a unary system along with elements of type @n@.
class CancelUnary u where
    -- | Yields the inverse operation of the one given.
    cancel :: u -> Maybe u

data TrigUnary = Sin | Cos | Tan | ASin | ACos | ATan deriving (Show, Eq, Ord)

instance (Floating a) => UnaryMock TrigUnary a where
    evalUnary Sin = sin
    evalUnary Cos = cos
    evalUnary Tan = tan
    evalUnary ASin = asin
    evalUnary ACos = acos
    evalUnary ATan = atan

instance CancelUnary TrigUnary where
    cancel uOp = 
        Just $ case uOp of
            Sin -> ASin
            Cos -> ACos
            Tan -> ATan
            ASin -> Sin
            ACos -> Cos
            ATan -> Tan

instance Renderable TrigUnary where
    render Sin = "sin"
    render Cos = "cos"
    render Tan = "tan"
    render ASin = "arcsin"
    render ACos = "arccos"
    render ATan = "arctan"