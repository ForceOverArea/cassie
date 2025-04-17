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
module Data.Cassie.Substitute 
    ( substitute
    , traverseTowards
    , rebuildOnto
    , AlgCrumb(..)
    , Substitute
    , SubstitutionError
    ) where

import safe Data.List
import safe Control.Monad.State (get, lift, put, runStateT, StateT)
import safe Control.Monad.Except (runExcept, throwError, Except)
import safe Data.Cassie.Structures (AlgebraicStruct(..), Symbol)
import safe Data.Cassie.Internal (insertAt, truthTable2)
import safe Data.Cassie.Isolate ((~?))

-- | 
data SubstitutionError 
    = IncorrectNumOfTermsToRebuild [AlgebraicStruct] (Maybe AlgCrumb)
    | IncorrectNumOfTermsToRebuildDelimited [AlgebraicStruct] Int Int 
    | IncorrectNumOfTermsToRebuildBinary (Maybe AlgebraicStruct)
    | FoundWrongSymbol
    | BadStructure
    | BinaryStructureContainedNone
    | NotDelimitedStructure
    | NotBinaryStructure

instance Show SubstitutionError where
    show (IncorrectNumOfTermsToRebuild structures (Just crumb))
        = "cannot rebuild structure onto structure(s) '" 
        ++ (intercalate ", " $ map show structures)
        ++ "' because that would be an invalid base structure for '"
        ++ show crumb
        ++ "'"

    show (IncorrectNumOfTermsToRebuild structures Nothing)
        = "cannot rebuild structure onto terms '"
        ++ (intercalate ", " $ map show structures)
        ++ "' because the result of a substitution must\
        \ be a single structure."

    show (IncorrectNumOfTermsToRebuildDelimited structures expected actual)
        = "cannot build a delimited structure onto '"
        ++ (intercalate ", " $ map show structures)
        ++ "' because the number of structures given did\
        \ not match the number of terms that needed to be\
        \ replaced in the parent structure. (expected "
        ++ show expected
        ++ ", but found "
        ++ show actual
        ++ ")"

    show (IncorrectNumOfTermsToRebuildBinary (Just otherTerm))
        = "could not build a binary structure because '"
        ++ show otherTerm
        ++ "' can only form a binary structure when built\
        \ onto EXACTLY one other structure"

    show (IncorrectNumOfTermsToRebuildBinary Nothing)
        = "could not build a binary structure because\
        \ a binary structure can only be built onto EXACTLY\
        \TWO other structures" 

    show FoundWrongSymbol 
        = "found a different symbol while traversing an\
        \ algebraic structure that claimed it contained\
        \ the symbol of interest"

    show BadStructure 
        = "found NO child structures while traversing an\
        \ algebraic structure that should have at least one"

    show BinaryStructureContainedNone 
        = "found a binary structure that claimed to contain\
        \ the symbol of interest, but whose two child structures\
        \ did not"

    show NotDelimitedStructure 
        = "tried to build a delimited algebraic structure using\
        \ a breadcrumb for a different structure"

    show NotBinaryStructure 
        = "tried to build a binary algrbraic structure using\
        \ a breadcrumb for a different structure"

-- | Breadcrumb type for an AlgebraicStruct zipper implementation.
--   Can represent delimited structures or binary operations (e.g. 
--   sums, or exponents, respectively)
data AlgCrumb
    = Delimited { kind    :: AlgebraicStruct        -- ^ The kind of structure that this should be
                , items   :: [AlgebraicStruct]      -- ^ The other structures in the delimited list
                , indices :: [Int]                  -- ^ The indices that the child structure should be inserted at
                }
    | Binary    { kind    :: AlgebraicStruct        -- ^ The kind of structure that this should be
                , other   :: Maybe AlgebraicStruct  -- ^ The other structure in this binary algebraic structure
                , hasLeft :: Bool                   -- ^ Specifes if this structure's `other` is the left constructor argument
                }
    | Singular  { kind    :: AlgebraicStruct        -- ^ The kind of structure that this structure contains
                }
    deriving Show

-- | The @Substitute@ zipper monad type for statefully traversing 
--   an @AlgebraicStruct@ parser tree.
type Substitute = StateT [AlgCrumb] (Except SubstitutionError)

-- | Pushes a crumb onto the stack of breadcrumbs in the substitute 
--   zipper monad.
pushCrumb :: AlgCrumb -> Substitute ()
pushCrumb c = get >>= put . (c:)

-- | Pops a crumb off the stack of breadcrumbs in the substitute 
--   zipper monad.
popCrumb :: Substitute (Maybe AlgCrumb)
popCrumb = do
    ccs <- get
    case uncons ccs of
        Nothing -> return Nothing
        Just (c, cs) -> do 
            put cs
            return $ Just c

-- | Substitutes occurrences of @target@ with @replacement@ wherever 
--   they found within @source@.
substitute :: Symbol -> AlgebraicStruct -> AlgebraicStruct -> Either SubstitutionError AlgebraicStruct
substitute target replacement source = runExcept $ fst <$> runStateT (substituteMain target replacement source) []

-- | The main 
substituteMain :: Symbol -> AlgebraicStruct -> AlgebraicStruct -> Substitute AlgebraicStruct
substituteMain target replacement source = do
    children <- traverseTowards target source
    case children of
        [] -> lift $ throwError BadStructure
        [Symbol s]
            | target == s -> rebuildOnto $ pure replacement
            | otherwise   -> lift $ throwError FoundWrongSymbol
        substructures -> do
            newBases <- mapM (substituteMain target replacement) substructures
            rebuildOnto newBases

-- | Moves the substitution zipper towards a given symbol in the given algebraic structure.
--   This may return multiple substructures containing the symbol.
traverseTowards :: Symbol -> AlgebraicStruct -> Substitute [AlgebraicStruct]
traverseTowards s struct = 
    case struct of
        Sum ts        -> traverseTowardsDelimited s struct ts
        Difference ts -> traverseTowardsDelimited s struct ts
        Product fs    -> traverseTowardsDelimited s struct fs
        Function _ _  -> traverseTowardsDelimited s struct $ argv struct
        Quotient _ _  -> traverseTowardsBinary s struct (dividend struct) $ divisor struct
        Exponent _ _  -> traverseTowardsBinary s struct (base struct) $ expn struct
        Logarithm _ _ -> traverseTowardsBinary s struct (base struct) $ logm struct
        Value _       -> return []
        Symbol s'     -> if s == s' then return [Symbol s'] else return []
        Group g       -> do
            pushCrumb $ Singular struct
            return [g]

-- | The logic needed to traverse the substitution zipper down towards a target symbol in an
--   algebraic substructure that has 1 or more delimited arguments (e.g. sum, product.)
traverseTowardsDelimited :: Symbol -> AlgebraicStruct -> [AlgebraicStruct] -> Substitute [AlgebraicStruct]
traverseTowardsDelimited s k terms = 
    let
        f = map $ flip elemIndices terms
    in do 
        let (has, doesnt) = partition (~? s) terms
        let hasIndices = concat $ f has
        pushCrumb $ Delimited k doesnt hasIndices
        return has

traverseTowardsBinary :: Symbol -> AlgebraicStruct -> AlgebraicStruct -> AlgebraicStruct -> Substitute [AlgebraicStruct]
traverseTowardsBinary s k l r = 
    let 
        both      = pushCrumb (Binary k Nothing False) >> return [l, r]
        neither   = lift $ throwError BinaryStructureContainedNone
        leftOnly  = pushCrumb (Binary k (Just r) False) >> return [l]
        rightOnly = pushCrumb (Binary k (Just l) True) >> return [r]
    in truthTable2 (~? s) l r both neither leftOnly rightOnly

-- | 'Zips up' a zipper onto a given algebraic structure, returnin the reconstructed tree struture.
rebuildOnto :: [AlgebraicStruct] -> Substitute AlgebraicStruct
rebuildOnto structures = do
    crumb <- popCrumb
    case (crumb, structures) of
        (Nothing, [x])        -> return x
        (Just crumbKind, x:_) -> do
            newBase <- case crumbKind of 
                Delimited k terms idxs   -> rebuildDelimited structures k terms idxs
                Binary k possTerm isLeft -> rebuildBinary structures k possTerm isLeft
                Singular k               -> rebuildSingular k x
            rebuildOnto $ pure newBase
        _ -> lift . throwError $ IncorrectNumOfTermsToRebuild structures crumb

-- | Rebuilds an 'unzipped' structure by zipping @baseTerms@ and @idxs@ into a Map-like 
--   structure, then using that map to insert the given @baseTerms@ into their respective 
--   spots in the algebraic structure being rebuilt.
rebuildDelimited :: [AlgebraicStruct] -> AlgebraicStruct -> [AlgebraicStruct] -> [Int] -> Substitute AlgebraicStruct
rebuildDelimited baseTerms structKind structTerms idxs =
    let 
        terms = foldl rebuildTerm structTerms $ zip idxs baseTerms
        rebuildTerm ts (idx, term) = insertAt term idx ts
        numIdxs = length idxs
        numTerms = length baseTerms
    in if numIdxs /= numTerms then
            lift . throwError $ IncorrectNumOfTermsToRebuildDelimited baseTerms numIdxs numTerms
        else do
            f <- constructOneDelimited structKind
            return $ f terms

rebuildBinary :: [AlgebraicStruct] -> AlgebraicStruct -> Maybe AlgebraicStruct -> Bool -> Substitute AlgebraicStruct
rebuildBinary baseArgs structKind possTerm isLeft = do
    f <- constructOneBinary structKind
    case (possTerm, baseArgs) of 
        (Nothing, [x, y]) -> return $ f x y
        (Just term, [x])
            | isLeft    -> return $ f term x 
            | otherwise -> return $ f x term
        _ -> lift . throwError $ IncorrectNumOfTermsToRebuildBinary possTerm
        
rebuildSingular :: AlgebraicStruct -> AlgebraicStruct -> Substitute AlgebraicStruct
rebuildSingular _structKind baseTerm = return $ Group baseTerm

constructOneDelimited :: AlgebraicStruct -> Substitute ([AlgebraicStruct] -> AlgebraicStruct)
constructOneDelimited x = 
    case x of
        Sum _        -> return Sum 
        Difference _ -> return Difference 
        Product _    -> return Product 
        Function n _ -> return $ Function n
        _            -> lift $ throwError NotDelimitedStructure

constructOneBinary :: AlgebraicStruct -> Substitute (AlgebraicStruct -> AlgebraicStruct -> AlgebraicStruct)
constructOneBinary x =
    case x of 
        Quotient _ _  -> return Quotient
        Exponent _ _  -> return Exponent
        Logarithm _ _ -> return Logarithm
        _             -> lift $ throwError NotBinaryStructure
