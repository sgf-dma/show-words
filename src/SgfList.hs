
module SgfList
    ( BState (..)
    , Index
    , indBase
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

import qualified Data.Foldable as F
import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader
import System.Random (RandomGen, randomRs)
import Data.Monoid

-- Reimplement list indexing part of Data.List using Backward State monad. And
-- some more useful functions for lists using State monads.


foldrM :: (Monad m) => (a -> b -> m b) -> b -> [a] -> m b
foldrM _ z []         = return z
foldrM g z (x : xs)   = foldrM g z xs >>= g x
{-
foldrM :: (F.Foldable t, Monad m) => (a -> b -> m b) -> b -> t a -> m b
foldrM g z xs       = F.foldr (\x mz -> mz >>= g x) (return z) xs-}
{-
foldlM :: (F.Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m b
foldlM _ z []         = return z
foldlM g z (x : xs)   = g z x >>= \z' -> foldlM g z' xs-}
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
elemsByIndsM :: (F.Foldable t) => [Index] -> t a -> BState Index [a]
elemsByIndsM js     = F.foldrM (\x -> BState . onlyInds (`elem` js) x) []

-- Complement to elemsByInds. Find all elements with _not_ specified indexes.
elemsByNotIndsM :: (F.Foldable t) => [Index] -> t a -> BState Index [a]
elemsByNotIndsM js  = F.foldrM (\x -> BState . onlyInds (`notElem` js) x) []

-- Reverse of elemsByInds. Find all indexes of specified elements.
indsByElemsM :: (F.Foldable t, F.Foldable t1) =>
                (a -> a -> Bool) -> t1 a -> t a -> BState Index [Index]
indsByElemsM eq ks  = F.foldrM (\x -> BState . onlyElems p x) []
  where p x         = F.any (`eq` x) ks

-- Unwrap monad from list indexing functions.
elemsByInds :: (F.Foldable t) => t a -> [Index] -> [a]
elemsByInds xs js       = fst . runBState (elemsByIndsM js xs) $ indBase

elemsByNotInds :: (F.Foldable t) => t a -> [Index] -> [a]
elemsByNotInds xs js    = fst . runBState (elemsByNotIndsM js xs) $ indBase

indsByElems :: (F.Foldable t, F.Foldable t1) =>
               (a -> a -> Bool) -> t a -> t1 a -> [Index]
indsByElems eq xs ks    = fst . runBState (indsByElemsM eq ks xs) $ indBase

-- Some more specific "instances" of above functions.
elemByInd :: (F.Foldable t) => t a -> Index -> [a]
elemByInd xs j      = elemsByInds xs [j]

indsByElem :: (F.Foldable t) => (a -> a -> Bool) -> t a -> a -> [Index]
indsByElem eq xs k  = indsByElems eq xs [k]

-- Convert list of elements into list of corresponding indexes in "reference"
-- list (preserving order of elements).
elemsOrder :: (F.Foldable t, F.Foldable t1) =>
              (a -> a -> Bool) -> t a -> t1 a -> [Index]
elemsOrder eq       = F.concatMap . indsByElem eq


-- Sublists.
--
-- Drop elements, that satisfies predicate p, starting from the list end.
dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd p      = foldr (\x z -> if null z && p x then [] else x : z) []

-- Split list by list (separator is list of elements). Does not omit separator
-- itself, just leave it as a separate element.  Note, that this function
-- can't be implemented using Backward State monad.
splitBy :: (F.Foldable t, Alternative f, Eq a) => [a] -> t a -> [f a]
splitBy ks          = let ks' = reverse ks
                      in  fst
                            . flip runState ks'
                            . flip runReaderT ks'
                            . splitByM

splitByM :: (F.Foldable t, Alternative f, Eq a) =>
            t a -> ReaderT [a] (State [a]) [f a]
splitByM xs         = do
                        (z1 : z2 : zs) <- F.foldrM fM [empty, empty] xs
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

-- Apply user-defined function g to every element in input list. Then evaluate
-- State of its result, and if it's True, mappend new element (returned by g)
-- into head of accumulator, otherwise, just add it to the accumulator (list).
foldrMerge :: (F.Foldable t, Monad m, Monoid b) => 
              (a -> State Bool (m b)) -> t a -> m [b]
foldrMerge g        = F.foldrM (\x zs -> let (mx', p) = runState (g x) False
                                       in  mx' >>= return . f p zs) []
  where
    f _ [] x'       = [x']
    f p (z : zs) x'
      | p           = x' `mappend` z : zs
      | otherwise   = x' : z : zs

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
transp :: (a -> a -> Bool) -> [a] -> [a] -> [a]
transp eq ks xs     = fst $ runBState (transpM eq xs) ks

transpM :: (a -> a -> Bool) -> [a] -> BState [a] [a]
transpM eq          = F.foldrM (\x -> BState . f x) []
  where
    --f :: a -> [a] -> [a] -> ([a], [a])
    f _ _  []           = ([], [])
    f x zs ks
      | x `elem'` ks    = (x : zs, filter (not . (`eq` x)) ks)
      | otherwise       = (zs, ks)
      where
        -- FIXME: Replace with any.
        --elem' :: a -> [a] -> Bool
        elem' k     = foldr (\y z -> (y `eq` k) || z) False


-- Random.
--
-- Shuffle list elements.
shuffleList :: RandomGen g => g -> [a] -> [a]
shuffleList g xs    = let lx = length xs
                          ts = transp (==) (take lx [1..]) $ randomRs (1, lx) g
                      in  ts >>= elemByInd xs

