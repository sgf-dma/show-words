{-# LANGUAGE TupleSections #-}

module SgfList
    ( BState (..)
    , ZipList' (..)
    , Index
    , indBase
    , foldrM
    , foldlM
    , elemByInd
    , elemsByInds
    , elemsByNotInds
    , indByElem
    , indsByElems1
    , indsByElems
    , indsByNotElems
    , elemsOrder1
    , elemsOrder
    , dropWhileEnd
    , splitBy
    , foldrMerge
    , concatSeq
    , zipWith'
    , zipMap
    , zipFoldM
    , listEq
    , transp
    , shuffleList
    )
  where

import Data.Maybe (fromMaybe)
import Data.Monoid
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader
import System.Random (RandomGen, randomRs)

-- Reimplementation of list indexing part of Data.List using Backward State
-- monad. And some other useful functions for lists using State monads.

foldrM :: (Monad m) => (a -> b -> m b) -> b -> [a] -> m b
foldrM _ z []         = return z
foldrM g z (x : xs)   = foldrM g z xs >>= g x
foldlM :: (Monad m) => (b -> a -> m b) -> b -> [a] -> m b
foldlM _ z []         = return z
foldlM g z (x : xs)   = g z x >>= \z' -> foldlM g z' xs

-- Backward state monad from "The essence of functional programming" by Philip
-- Wadler.
newtype BState s a  = BState {runBState :: (s -> (a, s))}
instance Monad (BState s) where
    return x        = BState (\s -> (x, s))
    BState m >>= f  = BState $ \s2 ->
                        let (x, s0)   = m s1
                            BState m' = f x
                            (x', s1)  = m' s2
                        in  (x', s0)

-- Slightly different ZipList: it uses my zipWith' instead of zipWith.  Note,
-- that it can't be made an instance of Applicative.
newtype ZipList' a  = ZipList' {getZipList' :: [a]}
  deriving (Show)
instance Monoid a => Monoid (ZipList' a) where
    mempty          = ZipList' []
    (ZipList' xs) `mappend` (ZipList' ys)
                    = ZipList' (zipWith' mappend xs ys)


-- Index list.
--
type Index          = Int
-- Start index.
indBase :: Index
indBase             = 1

-- FIXME: For v3. Make elemsByInds and indsByElems preserving indexes (keys)
-- order.  I.e. elements in resulting list must be on the positions
-- corresponding to their keys.  Implement this using zipper. This also should
-- make unnecessary construction like
--
--      \xs -> order >>= flip elemByInd xs
--
-- and speed it up dramatically.
-- FIXME: Versions, which work for several indexes (keys), and may have
-- several results (e.g. indsByElems) should return in Alernative. This allows
-- to not provide special version for infinity lists, because Alternative may
-- be defined in such way, that (<|>) ignores 2nd argument. So, will function
-- work on infinity list or not will be completely defined by Alternative
-- instance definition.

-- Folding functions for use in State monads for indexing list.
-- 
-- If all elements from key list ks satisfy predicate p, add x to the result
-- zs. This folding function will not work on infinity list.
allElems :: ([a] -> Bool) -> [a] -> b -> [b] -> ([b], [a])
allElems p ks x zs
  | p ks            = (x : zs, ks)
  | otherwise       = (zs, ks)

-- If at least one element from key list ks satisfies predicate p, add x to
-- the result zs and remove matched key from ks (so each key can match only
-- once). This version (unlike allElems) has fixed point, when there is no
-- more keys left, and, hence, will work on infinity list.
anyElems :: (a -> Bool) -> [a] -> b -> [b] -> ([b], [a])
anyElems _ [] _ _   = ([], [])  -- fixed point.
anyElems p ks x zs
  | any p ks        = (x : zs, filter (not . p) ks)
  | otherwise       = (zs, ks)


-- Index list by right folding it inside Backward State monad.
--
-- I assume, that keys (or indexes) list is _not_ sorted and for every key
-- (index) i should check every element of keys (indexes) list. Hence, if keys
-- (indexes) list is infinity and key (index) does not present in it (or
-- already have been found and deleted), all below functions will search
-- indefinitely. Though, some of them can work on infinity input list.
--
-- Find all elements with specified indexes. Works on infinity input list.
elemsByInds :: [Index] -> [a] -> [a]
elemsByInds js      = fst . flip runBState (indBase, js) . elemsByIndsM

elemsByIndsM :: [a] -> BState (Index, [Index]) [a]
elemsByIndsM        = foldrM (\x -> BState . f x) []
  where
    f x zs (s, js)  = fmap (s + 1, ) $ anyElems (s ==) js x zs

-- Complement to elemsByInds. Find all elements with indexes _not_ in the
-- supplied list.  Does not work on infinity input list.
elemsByNotInds :: [Index] -> [a] -> [a]
elemsByNotInds js   = fst . flip runBState (indBase, js) . elemsByNotIndsM

elemsByNotIndsM :: [a] -> BState (Index, [Index]) [a]
elemsByNotIndsM     = foldrM (\x -> BState . f x) []
  where
    f x zs (s, js)  = fmap (s + 1, ) $ allElems (all (s /=)) js x zs

-- Reverse of elemByInds. Find _first_ index of all elements in the supplied
-- list.  Works on infinity input list.
indsByElems1 :: (a -> a -> Bool)
            -> [a]  -- Elements, which indexes i'm searching for.
            -> [a]  -- List, where i'm searching for.
            -> [Index]
indsByElems1 eq ks   = fst . flip runBState (indBase, ks) . indsByElemsM1 eq

indsByElemsM1 :: (a -> a -> Bool) -> [a] -> BState (Index, [a]) [Index]
indsByElemsM1 eq    = foldrM (\x -> BState . f x) []
  where
    f x zs (s, ks)  = fmap (s + 1, ) $ anyElems (x `eq`) ks s zs

-- Find _all_ indexes of all elements in the supplied list.  Does not work on
-- infinity input list.
indsByElems :: (a -> a -> Bool)
            -> [a]  -- Elements, which indexes i'm searching for.
            -> [a]  -- List, where i'm searching for.
            -> [Index]
indsByElems eq ks   = fst . flip runBState (indBase, ks) . indsByElemsM eq

indsByElemsM :: (a -> a -> Bool) -> [a] -> BState (Index, [a]) [Index]
indsByElemsM eq     = foldrM (\x -> BState . f x) []
  where
    f _ _  (s, [])  = fmap (s + 1, ) ([], [])  -- fixed point.
    f x zs (s, ks)  = fmap (s + 1, ) $ allElems p ks s zs
      where p = any (x `eq`)

-- Complement to indsByElems. Find all indexes of elements _not_ in the
-- supplied list.  Does not work on infinity input list.
indsByNotElems :: (a -> a -> Bool)
               -> [a]  -- Elements, which indexes i'm searching for.
               -> [a]  -- List, where i'm searching for.
               -> [Index]
indsByNotElems eq ks = fst . flip runBState (indBase, ks) . indsByNotElemsM eq

indsByNotElemsM :: (a -> a -> Bool) -> [a] -> BState (Index, [a]) [Index]
indsByNotElemsM eq  = foldrM (\x -> BState . f x) []
  where
    f x zs (s, ks)  = fmap (s + 1, ) $ allElems p ks s zs
      where p = all (not . (x `eq`))


-- Some more specific "instances" of above functions.
--
-- Works on infinity list.
elemByInd :: Index -> [a] -> [a]
elemByInd j         = elemsByInds [j]

-- Works on infinity list.
indByElem :: (a -> a -> Bool)
            -> a     -- Element, which index i'm searching for.
            -> [a]   -- List, where i'm searching for.
            -> [Index]
indByElem eq k      = indsByElems1 eq [k]

-- Does not work on infinity list.
indsByElem :: (a -> a -> Bool)
           -> a     -- Element, which index i'm searching for.
           -> [a]   -- List, where i'm searching for.
           -> [Index]
indsByElem eq k     = indsByElems eq [k]

-- Convert list of elements into list of _first_ element's indexes in
-- "reference" list. Indexes comes in the same order as elements, which i have
-- searched for (instead of indexes in increasing order, which result of
-- indsByElems will have).
elemsOrder1 :: (a -> a -> Bool)
            -> [a]   -- Elements, which indexes i'm searching for.
            -> [a]   -- List, where i'm searching for.
            -> [Index]
elemsOrder1 eq ks xs = ks >>= flip (indByElem eq) xs

-- Convert list of elements into list of _all_ element's indexes in
-- "reference" list. Indexes comes in the same order as elements, which i have
-- searched for (instead of indexes in increasing order, which result of
-- indsByElems will have).
elemsOrder :: (a -> a -> Bool)
           -> [a]   -- Elements, which indexes i'm searching for.
           -> [a]   -- List, where i'm searching for.
           -> [Index]
elemsOrder eq ks xs = ks >>= flip (indsByElem eq) xs


-- Sublists.
--
-- Drop elements, that satisfies predicate p, starting from the list end.
dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd p      = foldr (\x z -> if null z && p x then [] else x : z) []

-- Split list by list (separator is list of elements). Does not omit separator
-- itself, just leave it as a separate element.  Note, that this function
-- can't be implemented using Backward State monad.
splitBy :: (Alternative f, Eq a) => [a] -> [a] -> [f a]
splitBy ks          = let ks' = reverse ks
                      in  fst
                            . flip runState ks'
                            . flip runReaderT ks'
                            . splitByM

splitByM :: (Alternative f, Eq a) =>
            [a] -> ReaderT [a] (State [a]) [f a]
splitByM xs         = do
                        (z1 : z2 : zs) <- foldrM fM [empty, empty] xs
                        return ((z1 <|> z2) : zs)
  where
    -- z1 is "probably separator", z2 is column behind the separator.
    --fM :: (Alternative f, Eq a) =>
    --      a -> [f a] -> ReaderT [a] (State [a]) [f a]
    fM x (z1 : z2 : zs) = do
                            ks <- ask
                            cs <- lift get
                            let (zs', cs') = f ks cs
                            lift (put cs')
                            return zs'
      where
        --f :: Eq a => [a] -> [a] -> ([f a], [a])
        f [] _          = (empty : (pure x <|> z2) : zs, [])
        f ks [c]
          | x == c      = (empty : empty : (pure x <|> z1) : z2 : zs, ks)
        f (k : ks) (c : cs)
          | x == c      = ((pure x <|> z1) : z2 : zs, cs)
          | x == k      = (pure x : (z1 <|> z2) : zs, ks)
          | otherwise   = (empty  : (pure x <|> z1 <|> z2) : zs, k : ks)


-- Folding.
--
-- Apply user-defined function g to every element in input list. Then if it
-- returns True along with new element (monoid), mappend new element into head
-- of accumulator, otherwise just add it to the accumulator list.
foldrMerge :: (Monad m, Monoid b) => (a -> m (b, Bool)) -> [a] -> m [b]
foldrMerge g        = foldrM (\x zs -> g x >>= return . f zs) []
  where
    f :: (Monoid b) => [b] -> (b, Bool) -> [b]
    f [] (x', _)    = [x']
    f (z : zs) (x', p)
      | p           = x' `mappend` z : zs
      | otherwise   = x' : z : zs

-- Sequence Traversable, then concatenate Foldable result in monad m.
concatSeq :: (T.Traversable t, Monad m) => t (m [a]) -> m [a]
concatSeq           = liftM F.concat . T.sequence


-- Zipping.
--
-- Instead of discarding longer tail (like zipWith do), add it to the result
-- as is.
zipWith' :: (a -> a -> a) -> [a] -> [a] -> [a]
zipWith' _ xs []                = xs
zipWith' _ [] ys                = ys
zipWith' f (x : xs) (y : ys)    = f x y : zipWith' f xs ys

-- FIXME: zipMap should return (Maybe (t b)) and use (StateT s Maybe) to
-- handle failure properly.
-- FIXME: Empty (or not long enough) list of functions is exception for
-- zipMap. To properly fix this, i need a way for function g to somehow
-- continue returning (m b) values without having (a -> b) function. In other
-- words, it either need data constructor for type b (which is impossible) or
-- b should be monoid (then it can use (return mempty)) or it may use last
-- function (a -> b) for all remaining elements (which still will break, if [a
-- -> b] is empty list). So, the only good solution is (Monoid b) constraint,
-- but i'm not sure, that i want it. Other way is implementing monadic zipMap
--
--      zipMapM :: (T.Traversable t, Monad m, Monoid (m b)) =>
--                 [a -> m b] -> t a -> m (t b)
--
-- Note, that (MonadPlus m) is not the right thing here, because mzero is
-- failure, but not a neutral context.

-- "Zippy apply (map)" list of function to some traversable datatype. If list
-- of functions [a -> b] is empty or not long enough, this is error.
zipMap :: (T.Traversable t) => [a -> b] -> t a -> t b
zipMap fs           = fst . flip runState fs . T.mapM g
  where
    g x             = do
                        (f : fs') <- get
                        put fs'
                        return (f x)

-- FIXME: Am i really need monadic zipFold ? Or just slightly different
-- zipMap, which folds result (i.e. does not have constraint T.Traversable)?
-- And, in such case, should i use (StateT s Maybe) here to just reflect
-- possible failure, nothing more? In other words, constraint MonadPlus seems
-- useless, since i don't need MonadPlus, i just need failure due to
-- not-equal-length lists (as well as in zipMap), and, hence, why not just use
-- Maybe?

-- Generic list Eq instance or "zippy monadic foldMap". List of monadic
-- functions [a -> m b] is "zippy applied" to other list [a] and results are
-- (right) folded in monad m into monoid b. If length of list of functions
-- [a -> m b] and other list [a] are not equal, return failure context of
-- monad m (mzero). If both have length zero, return mempty in monad m. Note,
-- that there is no sense here in
--      
--      (F.Foldable t) => .. -> t a -> ..
--
-- because any foldable can be converted (foldMap-ed) into list, and then
-- applied to this function.
zipFoldM :: (MonadPlus m, Monoid b) => [a -> m b] -> [a] -> m b
zipFoldM fs xs      = do
                        (y, gs) <- runStateT (zipFoldrMT xs) fs
                        case gs of
                          [] -> return y
                          _  -> mzero

-- I need foldl here, because other zipFoldMT will not work, when either of
-- lists is infinity.
zipFoldlMT :: (MonadPlus m, Monoid b) => [a] -> StateT [a -> m b] m b
zipFoldlMT          = foldlM (\z -> StateT . g z) mempty
  where
    --g :: (MonadPlus m, Monoid b) =>
    --     a -> b -> [a -> m b] -> m (b, [a -> m b])
    g _ _ []        = mzero
    g z x (f : fs)  = do
                        y <- f x
                        return (y `mappend` z, fs)

-- I assume, that (y `mappend` z) works faster, than (z `mappend` y), where z
-- is "long" monoid (e.g. this is true for (++), which is mappend for list).
-- And here i don't want to mappend elements in reverse order (as in
-- zipFoldlMT), which is what happen, if i use (y `mappend` z) in foldl, where
-- z is fold accumulator.  So, i use function composition to sequence monoids
-- in correct (foldr's) order and then just apply resulting function to
-- neutral monoid - mempty.  See "Using Difference Lists" from "Learn You a
-- Haskell for Great Good" for details.
zipFoldrMT :: (MonadPlus m, Monoid b) => [a] -> StateT [a -> m b] m b
zipFoldrMT xs       = do
                        h <- foldlM (\z -> StateT . g z) (mempty `mappend`) xs
                        return (h mempty)
  where
    --g :: (MonadPlus m, Monoid b) =>
    --     a -> b -> [a -> m b] -> m (b, [a -> m b])
    g _ _ []        = mzero
    g z x (f : fs)  = do
                        y <- f x
                        return (z . (y `mappend`), fs)

-- Eq instance for foldable datatype with some function eq supplied as
-- equality for elements.
listEq :: (a -> a -> Bool) -> [a] -> [a] -> Bool
listEq eq xs        = let xs' = map (\x -> Just . All . (x `eq`)) xs
                      in  getAll . fromMaybe (All False) . zipFoldM xs'


-- Random.
--
-- Pick from a list xs first occurences of all elements found in reference
-- list ks.  Stop processing a list xs if all reference elements have found.
-- Works with inifinity list xs, if it contain all elements from reference
-- list ks.  May be used to make random transposition from randomRs output.
transp :: Eq a => [a] -> [a] -> [a]
transp ks           = fst . flip runBState ks . transpM

transpM :: Eq a => [a] -> BState [a] [a]
transpM             = foldrM (\x -> BState . f x) []
  where
    --f :: a -> [a] -> [a] -> ([a], [a])
    f _ _  []           = ([], [])
    f x zs ks
      | x `elem` ks     = (x : zs, filter (/= x) ks)
      | otherwise       = (zs, ks)

-- Shuffle list elements.
shuffleList :: RandomGen g => g -> [a] -> [a]
shuffleList g xs    = let lx = length xs
                          ts = transp (take lx [1..]) $ randomRs (1, lx) g
                      in  ts >>= flip elemByInd xs

