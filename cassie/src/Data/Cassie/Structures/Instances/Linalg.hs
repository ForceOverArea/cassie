{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Cassie.Structures.Instances.Linalg
    ( Mixed(..)
    , MixedMagma(..)
    , MixedUnary(..)
    ) where

import safe Control.Exception
import safe Data.Cassie.Structures.Magmas
import safe Data.Cassie.Structures.UnarySystems
import safe Data.Cassie.Utils
import Numeric.LinearAlgebra as NLA
import System.IO.Unsafe

type DoesNotExistReason = String

type MixedElem = Double

-- | A type for an "algebraic structure" over both 
--   matrices and scalars (just like engineering school).
--
--   There is a LOT of mental gymnastics being done to 
--   make this type make sense in this context.
data Mixed
    = Mtrx { mtrx :: (Matrix MixedElem) }
    | Sclr { sclr :: MixedElem }
    | DNE DoesNotExistReason
    deriving (Show, Eq)

data MixedMagma 
    = MixedExpn
    | MixedLogm
    | MixedRoot
    | CrossProd
    deriving (Show, Eq, Ord)

data MixedUnary
    = MtrxGet { getRow :: Int, getCol :: Int }
    | MtrxTrans
    | SclrTrig TrigUnary
    deriving (Show, Eq, Ord)

-- instance Show Mixed where

instance Ord Mixed where
    Sclr x <= Sclr y = x <= y
    Mtrx x <= Mtrx y = sumElements x <= sumElements y
    Mtrx x <= Sclr y = sumElements x <= y
    Sclr x <= Mtrx y = x <= sumElements y
    Mtrx _ <= DNE _  = False
    Sclr _ <= DNE _  = False
    DNE _ <= _       = True

instance Num Mixed where
    Sclr x + Sclr y = Sclr $ x + y
    Mtrx x + Mtrx y = Mtrx $ x + y
    _ + _ = DNE "addends must be of the same type"

    Sclr x * Sclr y = Sclr $ x * y
    Sclr x * Mtrx y = Mtrx $ scalar x * y
    Mtrx x * Sclr y = Mtrx $ scalar y * x
    Mtrx x * Mtrx y =
        let 
            xCols = cols x
            yRows = rows y
        in if xCols == yRows then
                Mtrx $ x NLA.<> y
            else 
                DNE "the left operand's columns must equal the right operand's rows"
    DNE r * _ = DNE r
    _ * DNE r = DNE r 

    negate (Sclr x) = Sclr $ negate x
    negate (Mtrx x) = Mtrx $ negate x
    negate x = x

    abs (Sclr x) = Sclr $ abs x
    abs (Mtrx x) = Mtrx $ abs x
    abs x = x

    signum (Sclr x) = Sclr $ signum x
    signum (Mtrx x) = Mtrx $ signum x
    signum x = x

    fromInteger = Sclr . fromInteger

instance Fractional Mixed where
    recip (Sclr x) = Sclr $ recip x

    {-  "...I need a way to harness his power and I think 
        I've found a way: that's right, we're gonna cheat."
        - AVGN

        Shoutout to hmatrix for only utilizing impure exceptions
        as their error-handling method of choice. As such, we
        need to undo that here. Let's catch the exception in a 
        small IO action and return DNE when inversion fails.
    -}
    recip (Mtrx x)
        | rows x /= cols x = DNE "only square matrices can be inverted"
        | det x == 0 = DNE "only matrices with a non-zero determinant can be inverted"
        | otherwise = Mtrx $ inv x

    recip x = x

    fromRational = Sclr . fromRational

instance Floating Mixed where
    pi  = Sclr pi
    exp = scalarOnly1 exp "exp only defined for scalars"
    log = scalarOnly1 log "log only defined for scalars"
    sin = scalarOnly1 sin "sin only defined for scalars"
    cos = scalarOnly1 cos "cos only defined for scalars"
    tan = scalarOnly1 tan "tan only defined for scalars"
    asin = scalarOnly1 asin "asin only defined for scalars" 
    acos = scalarOnly1 acos "acos only defined for scalars"
    atan = scalarOnly1 atan "atan only defined for scalars"
    sinh = scalarOnly1 sinh "sinh only defined for scalars"
    cosh = scalarOnly1 cosh "cosh only defined for scalars"
    tanh = scalarOnly1 tanh "tanh only defined for scalars"
    asinh = scalarOnly1 asinh "asinh only defined for scalars"
    acosh = scalarOnly1 acosh "acosh only defined for scalars"
    atanh = scalarOnly1 atanh "atanh only defined for scalars"

    Sclr x ** Sclr y = Sclr $ x ** y
    Mtrx x ** Sclr y = 
        if rows x /= cols x then
            DNE "only square matrices can be raised to a power"
        else 
            maybe 
                (DNE "matrices can only be raised to an integral power")
                (Mtrx . foldl' (NLA.<>) x . (flip replicate x) . (-) 1)
                $ realInt y
    _ ** _ = DNE "exponents can only be evaluated over scalars"

instance Transposable Mixed Mixed where
    tr (Mtrx m) = Mtrx $ tr m 
    tr _ = DNE "cannot conjugate transpose a scalar"

    tr' (Mtrx m) = Mtrx $ tr' m
    tr' _ = DNE "cannot transpose a scalar"

instance MagmaMock MixedMagma Mixed where
    evalMagma MixedExpn = mixedExp
    evalMagma MixedLogm = scalarOnly2 logBase "can only take the logarithm of scalar values"
    evalMagma MixedRoot = scalarOnly2 (flip (**) . (1.0 /)) "can only take the nth root of scalar values"
    evalMagma CrossProd = mixedCross

instance CancelMagma MixedMagma where
    lCancel CrossProd = Nothing
    lCancel mOp = 
        Just $ case mOp of
            MixedExpn -> Left MixedLogm
            MixedLogm -> Left MixedExpn
            MixedRoot -> Right MixedExpn 

    rCancel CrossProd = Nothing
    rCancel mOp = 
        Just $ case mOp of
            MixedExpn -> Left MixedRoot
            MixedLogm -> Right MixedRoot
            MixedRoot -> Left MixedLogm 

instance ShowMagma MixedMagma where
    showMagma MixedExpn = \x y -> show x ++ "^" ++ show y
    showMagma MixedLogm = \x y -> "log<" ++ show x ++ ">(" ++ show y ++ ")"
    showMagma MixedRoot = \x y -> "root<" ++ show x ++ ">(" ++ show y ++ ")"
    showMagma CrossProd = \x y -> show x ++ " >< " ++ show y

instance UnaryMock MixedUnary Mixed where
    evalUnary MtrxTrans = tr'
    evalUnary (MtrxGet gr gc) = getMtrxIndex gr gc
    evalUnary (SclrTrig trigOp) = evalUnary trigOp

instance CancelUnary MixedUnary where
    cancel (MtrxGet _ _) = Nothing
    cancel (SclrTrig trigOp) = SclrTrig <$> cancel trigOp
    cancel MtrxTrans = Just MtrxTrans

instance ShowUnary MixedUnary where
    showUnary (MtrxGet i j) = \x -> "get<" ++ show i ++ "," ++ show j ++ ">(" ++ show x ++ ")"
    showUnary (SclrTrig trigOp) = showUnary trigOp
    showUnary MtrxTrans = (++ "^T") . show

mixedCross :: Mixed -> Mixed -> Mixed
Mtrx x `mixedCross` Mtrx y = 
    let
        i = flatten x
        j = flatten y
        n = size i
    in if n == size j && n == 3 then 
        Mtrx . asColumn $ i `cross` j
    else 
        DNE "cross product is only defined for 3-element vectors"
mixedCross _ _ = DNE "cross product is only defined for 3-element vectors"

mixedExp :: Mixed -> Mixed -> Mixed
Sclr x `mixedExp` Sclr y = Sclr $ x ** y
Mtrx x `mixedExp` Sclr y = 
    case realInt y of
        Just n' -> Mtrx $ x ^^ n'
        Nothing -> DNE "matrix can only be raised to an integral power"
mixedExp _ _ = DNE "exponentiation only defined between real scalars or matrices and integers"

getMtrxIndex :: Int -> Int -> Mixed -> Mixed
getMtrxIndex i j (Mtrx x) = Sclr $ x ! i ! j
getMtrxIndex _ _ _ = DNE "only matrices can be indexed"

scalarOnly1 :: (MixedElem -> MixedElem) -> DoesNotExistReason -> Mixed -> Mixed
scalarOnly1 f _err (Sclr x) = (Sclr $ f x)
scalarOnly1 _f err _ = DNE err

scalarOnly2 :: (MixedElem -> MixedElem -> MixedElem) -> DoesNotExistReason -> Mixed -> Mixed -> Mixed
scalarOnly2 f _err (Sclr x) (Sclr y) = (Sclr $ f x y)
scalarOnly2 _f err _ _ = DNE err
