{-# LANGUAGE Safe #-}
module Data.Cassie.Substitute 
    ( substitute
    , traverseTowards
    , rebuildOnto
    , AlgCrumb(..)
    , Substitute
    , SubstitutionError
    ) where

import safe Control.Arrow ((***))
import safe Control.Monad (join, when)
import safe Control.Monad.State (get, lift, put, runStateT, StateT)
import safe Control.Monad.Except (runExcept, throwError, Except)
import safe Data.Cassie.Structures (AlgebraicStruct(..), Symbol)
import safe Data.Cassie.Internal (get2)
import safe Data.Cassie.Isolate ((~?))
import safe Data.List (elemIndices, partition, uncons)

data SubstitutionError 
    = NoCrumbsLeft
    | IncorrectNumOfTermsToRebuild
    | FoundWrongSymbol
    | BadStructure
    deriving Show

-- | Breadcrumb type for an AlgebraicStruct zipper implementation.
--   Can represent delimited structures or binary operations (e.g. sums, or exponents, respectively)
data AlgCrumb
    = Delimited 
        { kind    :: AlgebraicStruct        -- ^ The kind of structure that this should be
        , items   :: [AlgebraicStruct]      -- ^ The other structures in the delimited list
        , indices :: [Int]                  -- ^ The indices that the child structure should be inserted at
        }
    | Binary 
        { kind    :: AlgebraicStruct        -- ^ The kind of structure that this should be
        , other   :: Maybe AlgebraicStruct  -- ^ The other structure in this binary algebraic structure
        , hasLeft :: Bool                   -- ^ Specifes if this structure's `other` is the left constructor argument
        }
    | Singular
        { kind    :: AlgebraicStruct        -- ^ The kind of structure that this structure contains
        }
    deriving Show

type Substitute = StateT [AlgCrumb] (Except SubstitutionError)

substitute :: Symbol -> AlgebraicStruct -> AlgebraicStruct -> Either SubstitutionError AlgebraicStruct
substitute target replacement source = runExcept $ fst <$> runStateT (substituteMain target replacement source) []

substituteMain :: Symbol -> AlgebraicStruct -> AlgebraicStruct -> Substitute AlgebraicStruct
substituteMain target replacement source = do
    children <- traverseTowards target source
    case children of
        [] -> lift $ throwError BadStructure
        [Symbol s] -> if target == s then
                rebuildOnto $ pure replacement
            else
                lift $ throwError FoundWrongSymbol
        substructures -> do
            newBases <- mapM (substituteMain target replacement) substructures
            rebuildOnto newBases

-- | Moves the substitution zipper towards a given symbol in the given algebraic structure.
--   This may return multiple substructures containing the symbol.
traverseTowards :: Symbol -> AlgebraicStruct -> Substitute [AlgebraicStruct]
-- Moves towards a target symbol in a delimited expression
traverseTowards s (Sum terms)               = traverseTowardsDelimited s (Sum []) terms
traverseTowards s (Difference subtrahends)  = traverseTowardsDelimited s (Difference []) subtrahends
traverseTowards s (Product factors)         = traverseTowardsDelimited s (Product []) factors
traverseTowards s (Function n argv')        = traverseTowardsDelimited s (Function n []) argv'
-- Moves towards a target symbol in a binary expression
traverseTowards s (Quotient d d')           = traverseTowardsBinary s (Quotient blank blank) (d, d')
traverseTowards s (Exponent b e)            = traverseTowardsBinary s (Exponent blank blank) (b, e)
traverseTowards s (Logarithm b l)           = traverseTowardsBinary s (Logarithm blank blank) (b, l)
-- Moves towards a target symbol in a group expression (retains the indirection in the parser tree)
traverseTowards _ (Group g)                 = do { pushCrumb $ Singular (Group blank); return [g] }
-- Reports a found symbol if one exists in a terminal element of the parser tree
traverseTowards _ (Value _)                 = return []
traverseTowards s (Symbol s')               = if s == s' then return [Symbol s'] else return []

-- | The logic needed to traverse the substitution zipper down towards a target symbol in an
--   algebraic substructure that has 1 or more delimited arguments (e.g. sum, product.)
traverseTowardsDelimited :: Symbol -> AlgebraicStruct -> [AlgebraicStruct] -> Substitute [AlgebraicStruct]
traverseTowardsDelimited s k terms = do 
    let (has, doesnt) = partition (~? s) terms
    let hasIndices = concat $ f has
    pushCrumb $ Delimited k doesnt hasIndices
    return has
    where 
        f = map $ flip elemIndices terms

traverseTowardsBinary :: Symbol 
    -> AlgebraicStruct 
    -> (AlgebraicStruct, AlgebraicStruct) 
    -> Substitute [AlgebraicStruct]
traverseTowardsBinary s k (l, r) = do
    case join (***) (~? s) (l, r) of -- TODO: clean this up
        (True, True) -> do 
            pushCrumb $ Binary k Nothing False
            return [l, r]
        (True, False) -> do
            pushCrumb $ Binary k (Just r) False
            return [l]
        (False, True) -> do
            pushCrumb $ Binary k (Just l) True
            return [r] 
        (False, False) -> error "ligma balls"

-- | 'Zips up' a zipper onto a given algebraic structure, returnin the reconstructed tree struture.
rebuildOnto :: [AlgebraicStruct] -> Substitute AlgebraicStruct
rebuildOnto structures = do
    crumb <- popCrumb
    case crumb of
        Nothing -> return $ head structures
        Just x -> rebuildOnto . pure =<< case x of 
            (Delimited k terms idxs) -> rebuildDelimited structures k terms idxs
            (Binary k possTerm isLeft) -> rebuildBinary structures k possTerm isLeft
            (Singular k) -> rebuildSingular k $ head structures

-- | Rebuilds an 'unzipped' structure by zipping @baseTerms@ and @idxs@ into a Map-like 
--   structure, then using that map to insert the given @baseTerms@ into their respective 
--   spots in the algebraic structure being rebuilt.
rebuildDelimited :: [AlgebraicStruct] 
    -> AlgebraicStruct 
    -> [AlgebraicStruct] 
    -> [Int] 
    -> Substitute AlgebraicStruct
rebuildDelimited baseTerms structKind structTerms idxs = do
    when (length idxs /= length baseTerms) 
        $ throwError IncorrectNumOfTermsToRebuild
    let terms' = foldl f structTerms $ zip idxs baseTerms
    return $ constructOneDelimited structKind terms' 
    where 
        -- | produces a list of terms with the given `term` inserted at `idx`
        f :: [AlgebraicStruct] -> (Int, AlgebraicStruct) -> [AlgebraicStruct]
        f ts (idx, term) = insertAt term idx ts

rebuildBinary :: [AlgebraicStruct] 
    -> AlgebraicStruct 
    -> Maybe AlgebraicStruct 
    -> Bool 
    -> Substitute AlgebraicStruct
rebuildBinary baseArgs structKind possTerm isLeft =
    case possTerm of 
        Just term -> do
            when (length baseArgs /= 1) 
                $ throwError IncorrectNumOfTermsToRebuild
            return . uncurry (constructOneBinary structKind) $ if isLeft then
                (term, head baseArgs)
            else
                (head baseArgs, term)
        Nothing -> do
            when (length baseArgs /= 2)
                $ throwError IncorrectNumOfTermsToRebuild
            return . uncurry (constructOneBinary structKind) $ get2 baseArgs

rebuildSingular :: AlgebraicStruct -> AlgebraicStruct -> Substitute AlgebraicStruct
rebuildSingular _structKind baseTerm = return $ Group baseTerm

constructOneDelimited :: AlgebraicStruct -> [AlgebraicStruct] -> AlgebraicStruct
constructOneDelimited (Sum _) = Sum 
constructOneDelimited (Difference _) = Difference 
constructOneDelimited (Product _) = Product 
constructOneDelimited (Function n _) = Function n
constructOneDelimited _ = error "cannot build an algebraic structure from the given arguments"

constructOneBinary :: AlgebraicStruct -> AlgebraicStruct -> AlgebraicStruct -> AlgebraicStruct
constructOneBinary (Quotient _ _) = Quotient
constructOneBinary (Exponent _ _) = Exponent
constructOneBinary (Logarithm _ _) = Logarithm
constructOneBinary _ = error "cannot build a binary algebraic structure from the given arguments"

-- constructOneSingular :: AlgebraicStruct -> AlgebraicStruct -> AlgebraicStruct
-- constructOneSingular _ = Group

pushCrumb :: AlgCrumb -> Substitute ()
pushCrumb c = get >>= put . (c:)

popCrumb :: Substitute (Maybe AlgCrumb)
popCrumb = do
    ccs <- get
    case uncons ccs of
        Nothing -> return Nothing
        Just (c, cs) -> do 
            put cs
            return $ Just c

enumerate :: [a] -> [(Int, a)]
enumerate = foldl f []
    where
        f xs h = xs ++ [(length xs, h)]

insertAt :: a -> Int -> [a] -> [a]
insertAt e idx xs = (take idx xs) ++ (e:(drop idx xs))

blank :: AlgebraicStruct
blank = Value 0.0
