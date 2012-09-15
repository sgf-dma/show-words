
module SgfList
    ( BState (..)
    , ZipList' (..)
    , Index
    , indBase
    , foldrM
    , elemsByInds
    , elemsByNotInds
    , indsByElems
    , elemByInd
    , indsByElem
    , elemsOrder
    , dropWhileEnd
    , splitBy
    , foldrMerge
    , zipWith'
    , transp
    , shuffleList
    )
  where

import Data.Monoid
import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader
import System.Random (RandomGen, randomRs)

-- Reimplement list indexing part of Data.List using Backward State monad. And
-- some more useful functions for lists using State monads.

foldrM :: (Monad m) => (a -> b -> m b) -> b -> [a] -> m b
foldrM _ z []         = return z
foldrM g z (x : xs)   = foldrM g z xs >>= g x
{-
foldrM :: (Foldable t, Monad m) => (a -> b -> m b) -> b -> t a -> m b
foldrM g z xs       = foldr (\x mz -> mz >>= g x) (return z) xs-}
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
    x `mappend` y   = let (ZipList' xs) = x
                          ys = getZipList' y
                      in  ZipList' $ zipWith' mappend xs ys


-- Index list.
--
type Index          = Int
-- Start index.
indBase :: Index
indBase             = 1

-- Folding functions for use in State monads for indexing list.
-- 
-- Add list element x to the accumulator z only if its index s satisfies
-- predicate.
onlyInds :: (Index -> Bool) -> a -> [a] -> Index -> ([a], Index)
onlyInds p x z s
  | p s             = (x : z, s + 1)
  | otherwise       = (z, s + 1)

-- Reverse of onlyInds: add list index s to the accumulator only if its
-- element x satisfies predicate.
onlyElems :: (a -> Bool) -> a -> [Index] -> Index -> ([Index], Index)
onlyElems p x z s
  | p x             = (s : z, s + 1)
  | otherwise       = (z, s + 1)

-- Index list by right folding it inside Backward State monad.
--
-- Find all elements with specified indexes.
elemsByIndsM :: [Index] -> [a] -> BState Index [a]
elemsByIndsM js     = foldrM (\x -> BState . onlyInds (`elem` js) x) []

-- Complement to elemsByInds. Find all elements with _not_ specified indexes.
elemsByNotIndsM :: [Index] -> [a] -> BState Index [a]
elemsByNotIndsM js  = foldrM (\x -> BState . onlyInds (`notElem` js) x) []

-- Reverse of elemsByInds. Find all indexes of specified elements.
indsByElemsM :: (a -> a -> Bool)
             -> [a] -- Elements, which indexes i'm searching for.
             -> [a] -- List, where i'm searching for.
             -> BState Index [Index]
indsByElemsM eq ks  = foldrM (\x -> BState . onlyElems p x) []
  where
    p x             = any (`eq` x) ks

-- Unwrap monad from list indexing functions.
elemsByInds :: [Index] -> [a] -> [a]
elemsByInds js      = fst . flip runBState indBase . elemsByIndsM js

elemsByNotInds :: [Index] -> [a] -> [a]
elemsByNotInds js   = fst . flip runBState indBase . elemsByNotIndsM js

indsByElems :: (a -> a -> Bool)
            -> [a]  -- Elements, which indexes i'm searching for.
            -> [a]  -- List, where i'm searching for.
            -> [Index]
indsByElems eq ks   = fst . flip runBState indBase . indsByElemsM eq ks

-- Some more specific "instances" of above functions.
elemByInd :: Index -> [a] -> [a]
elemByInd j         = elemsByInds [j]

indsByElem :: (a -> a -> Bool)
           -> a     -- Element, which index i'm searching for.
           -> [a]   -- List, where i'm searching for.
           -> [Index]
indsByElem eq k     = indsByElems eq [k]

-- Convert list of elements into list of corresponding indexes in "reference"
-- list. Indexes comes in the same order as elements i have searched for,
-- instead of index increase order, which will have result of indsByElems.
elemsOrder :: (a -> a -> Bool)
           -> [a]   -- Elements, which indexes i'm searching for.
           -> [a]   -- List, where i'm searching for.
           -> [Index]
--elemsOrder eq ks xs = concatMap . indsByElem eq
--elemsOrder eq ks xs = concatMap (\k -> indsByElem eq k xs) $ ks
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
    f [] (x', _)    = [x']
    f (z : zs) (x', p)
      | p           = x' `mappend` z : zs
      | otherwise   = x' : z : zs

foldrMerge' :: (Monad m, Monoid b) => (a -> (m b, Bool)) -> [a] -> m [b]
foldrMerge' g       = foldrM (\(mx, p) zs -> mx >>= return . f p zs) [] . map g
  where
    f _ [] x'       = [x']
    f p (z : zs) x'
      | p           = x' `mappend` z : zs
      | otherwise   = x' : z : zs


-- Zipping.
--
-- Instead of discarding longer tail (like zipWith do), add it to the result
-- as is.
zipWith' :: (a -> a -> a) -> [a] -> [a] -> [a]
zipWith' _ xs []                = xs
zipWith' _ [] ys                = ys
zipWith' f (x : xs) (y : ys)    = f x y : zipWith' f xs ys

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


-- Random.
--
-- Shuffle list elements.
shuffleList :: RandomGen g => g -> [a] -> [a]
shuffleList g xs    = let lx = length xs
                          ts = transp (take lx [1..]) $ randomRs (1, lx) g
                      in  ts >>= flip elemByInd xs

