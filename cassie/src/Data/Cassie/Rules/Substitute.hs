{- |
Module      :  Substitute
Description :  Monad transformer stack and actions for substituting algebraic structures into another algebra
Copyright   :  (c) Grant Christiansen
License     :  MIT

Maintainer  :  christiansengrant18@gmail.com
Stability   :  [unstable] | experimental | provisional | stable | frozen
Portability :  portable

Defines the @Substitute@ monad transformer stack type and appropriate actions.
These are exposed for usage elsewhere, but are primarily meant to be used in the 
@substitute@ function, which can take one algebraic structure and, with context, 
replace occurrances of symbols within that context with algebraic structures that 
they represent in the context of a larger problem.
-}

{-# LANGUAGE Safe #-}
module Data.Cassie.Rules.Substitute 
    ( substitute
    , substituteFnArgs
    , SubstitutionError(..)
    ) where

import safe Control.Monad
import safe Control.Monad.State (get, put, runStateT, StateT)
import safe Control.Monad.Except (runExcept, Except)
import safe Data.List
import safe qualified Data.List.NonEmpty as NE
import safe Data.Cassie.Structures
import safe Data.Cassie.Utils

data SubstitutionError 
    = BadStructure
    | BinaryStructureContainsNone
    | FoundWrongSymbol
    | IncorrectNumOfTermsToRebuild Int Int
    | StructureIsNotSemigroup
    | UnaryStructureContainsNone
    | MalformedStructure
    deriving (Show, Eq, Ord)

data AlgCrumb mg u n
    = Plural    { kind    :: AlgStruct mg u n 
                , items   :: [AlgStruct mg u n]
                , indices :: [Int]
                }
    | MagmaOp   { magKind :: mg
                , other   :: Maybe (AlgStruct mg u n)
                , hasLeft :: Bool
                }
    | UnaryOp   { unyKind :: u
                }
    | Negate
    | Invert
    deriving (Show, Eq)

type Substitute mg u n = StateT [AlgCrumb mg u n] (Except SubstitutionError)

substitute :: AlgebraicStructure mg u n 
    => Symbol 
    -> AlgStruct mg u n 
    -> AlgStruct mg u n 
    -> Either SubstitutionError (AlgStruct mg u n)
substitute target replacement source = runExcept $ fst <$> runStateT (substituteMain target replacement source) []

substituteFnArgs :: AlgebraicStructure mg u n 
    => AlgStruct mg u n 
    -> [Symbol] 
    -> ([AlgStruct mg u n] -> Either SubstitutionError (AlgStruct mg u n))
substituteFnArgs impl args = 
    let 
        foldFunc = flip $ uncurry substitute
        subArgs  = foldM foldFunc impl
    in subArgs . zip args

substituteMain :: AlgebraicStructure mg u n => Symbol -> AlgStruct mg u n -> AlgStruct mg u n -> Substitute mg u n (AlgStruct mg u n) 
substituteMain target replacement source = do
    children <- traverseTowards target source
    case children of
        [] -> throwErr BadStructure
        [Symbol s]
            | target == s -> rebuildOnto $ pure replacement
            | otherwise   -> throwErr FoundWrongSymbol
        substructures -> do
            newBases <- mapM (substituteMain target replacement) substructures
            rebuildOnto newBases

traverseTowards :: AlgebraicStructure mg u n => Symbol -> AlgStruct mg u n -> Substitute mg u n [AlgStruct mg u n]
traverseTowards s struct =
    case struct of
        Additive ts         -> traverseTowardsPlural s struct $ NE.toList ts
        Multiplicative fs   -> traverseTowardsPlural s struct $ NE.toList fs
        Negated x           -> pushCrumb Negate >> pure [x]
        Inverse x           -> pushCrumb Invert >> pure [x]
        Magma m l r         -> traverseTowardsMagmaOp s m l r
        Unary u x           -> (:[]) <$> traverseTowardsUnaryOp u x
        N_ary _ args        -> traverseTowardsPlural s struct args 
        Nullary _           -> pure []
        Symbol s'           -> if s == s' then pure [Symbol s'] else pure []

traverseTowardsPlural :: AlgebraicStructure mg u n => Symbol -> AlgStruct mg u n -> [AlgStruct mg u n]-> Substitute mg u n [AlgStruct mg u n]
traverseTowardsPlural s k terms = 
    let 
        getArgPositionMap = map (flip elemIndices terms)
    in do
        let (has, doesnt) = partition (s ~?) terms
        let hasIndices = concat $ getArgPositionMap has
        pushCrumb $ Plural k doesnt hasIndices
        pure has

traverseTowardsMagmaOp :: Symbol -> mg -> AlgStruct mg u n -> AlgStruct mg u n -> Substitute mg u n [AlgStruct mg u n]
traverseTowardsMagmaOp s k l r = 
    let 
        both      = pushCrumb (MagmaOp k Nothing False) >> pure [l, r]
        neither   = throwErr BinaryStructureContainsNone
        leftOnly  = pushCrumb (MagmaOp k (Just r) False) >> pure [l]
        rightOnly = pushCrumb (MagmaOp k (Just l) True) >> pure [r]
    in truthTable2 (s ~?) l r both neither leftOnly rightOnly

traverseTowardsUnaryOp :: u -> AlgStruct mg u n -> Substitute mg u n (AlgStruct mg u n)
traverseTowardsUnaryOp k x = (pushCrumb $ UnaryOp k) >> pure x

rebuildOnto :: [AlgStruct mg u n] -> Substitute mg u n (AlgStruct mg u n)
rebuildOnto structures = do
    crumb <- popCrumb
    case (crumb, structures) of
        (Nothing, [x]) -> pure x
        (Just crumbKind, x:_) -> do
            newBase <- case crumbKind of
                Plural k terms idxs       -> rebuildPlural structures k terms idxs
                MagmaOp k possTerm isLeft -> rebuildMagmaOp structures k possTerm isLeft
                UnaryOp k                 -> rebuildUnaryOp structures k x
                Negate                    -> pure $ Negated x
                Invert                    -> pure $ Inverse x
            rebuildOnto $ pure newBase
        _ -> error $ show MalformedStructure

rebuildPlural :: [AlgStruct mg u n] -> AlgStruct mg u n -> [AlgStruct mg u n] -> [Int] -> Substitute mg u n (AlgStruct mg u n)
rebuildPlural baseTerms structKind structTerms idxs = 
    let 
        terms = foldl rebuildTerm structTerms $ zip idxs baseTerms
        rebuildTerm ts (idx, term) = insertAt term idx ts
        numIdxs = length idxs
        numTerms = length baseTerms
    in if numIdxs /= numTerms then
            throwErr $ IncorrectNumOfTermsToRebuild numIdxs numTerms
        else do
            f <- constructOnePlural structKind
            pure $ f terms

rebuildMagmaOp :: [AlgStruct mg u n] -> mg -> Maybe (AlgStruct mg u n) -> Bool -> Substitute mg u n (AlgStruct mg u n)
rebuildMagmaOp baseArgs structKind possTerm isLeft = do
    f <- constructOneMagmaOp structKind
    case (possTerm, baseArgs) of
        (Nothing, [x, y]) -> pure $ f x y
        (Just term, [x])
            | isLeft    -> pure $ f term x
            | otherwise -> pure $ f x term
        _ -> throwErr $ IncorrectNumOfTermsToRebuild 2 0 -- expected <= 2, found 0

rebuildUnaryOp :: [AlgStruct mg u n] -> u -> AlgStruct mg u n -> Substitute mg u n (AlgStruct mg u n)
rebuildUnaryOp _ structKind term = do
    f <- constructOneUnaryOp structKind 
    pure $ f term

constructOnePlural :: AlgStruct mg u n -> Substitute mg u n ([AlgStruct mg u n] -> AlgStruct mg u n)
constructOnePlural x = 
    case x of
        Additive _          -> pure $ Additive . NE.fromList
        Multiplicative _    -> pure $ Multiplicative . NE.fromList
        N_ary name _        -> pure $ N_ary name
        _ -> throwErr StructureIsNotSemigroup

constructOneMagmaOp :: mg -> Substitute mg u n (AlgStruct mg u n -> AlgStruct mg u n -> AlgStruct mg u n)
constructOneMagmaOp x = pure (Magma x)

constructOneUnaryOp :: u -> Substitute mg u n (AlgStruct mg u n -> AlgStruct mg u n)
constructOneUnaryOp x = pure (Unary x)

pushCrumb :: AlgCrumb mg u n -> Substitute mg u n ()
pushCrumb c = get >>= put . (c:)

popCrumb :: Substitute mg u n (Maybe (AlgCrumb mg u n))
popCrumb = do
    ccs <- get
    case uncons ccs of
        Nothing -> pure Nothing
        Just (c, cs) -> do
            put cs 
            pure $ Just c
