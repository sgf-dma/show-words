
module SgfListIndex
    ( Index
    , indBase
    , elemsByInds
    , elemsByNotInds
    , indsByElems
    , elemByInd
    , indsByElem
    , elemsOrder)
  where

import Data.Foldable (foldrM)

-- Index list.
type Index          = Int
-- Start index.
indBase             = 1
-- Backward state monad from "The essence of functional programming" by Philip
-- Wadler.
newtype BState s a  =  BState {runBState :: (s -> (a, s))}
instance Monad (BState s) where
    return x        =  BState (\s -> (x, s))
    BState m >>= f  =  BState $ \s2 ->
                        let (x, s0)   = m s1
                            BState m' = f x
                            (x', s1)  = m' s2
                        in  (x', s0)

-- Folding functions for use in State monads for indexing list.
-- 
-- Add list element x to the accumulator z only if its index s equals to i.
onlyInds :: [Index] -> a -> [a] -> Index -> ([a], Index)
onlyInds js x z     = \s -> let z' = if s `elem` js then x : z else z
                            in  (z', s + 1)

-- Complement to onlyInd. Add each list elements x to accumulator z if its
-- index s not equals to i.
notInds :: [Index] -> a -> [a] -> Index -> ([a], Index)
notInds js x z      = \s -> let z' = if s `notElem` js then x : z else z
                            in  (z', s + 1)

indsOfs :: (a -> [a] -> Bool) -> [a] -> a -> [Index] -> Index -> ([Index], Index)
indsOfs elem' ks x z  = \s -> let z' = if x `elem'` ks then s : z else z
                              in  (z', s + 1)

-- Index list by right folding it inside Backward State monad.
--
-- Choose all elements from list which indexes are in the specified index
-- list.
elemsByIndsM :: [Index] -> [a] -> BState Index [a]
elemsByIndsM js     = foldrM (\x -> BState . onlyInds js x) []

-- Complement to elemsByInds. Choose all elements from list which indexes are
-- not in the sepcified index list.
elemsByNotIndsM :: [Index] -> [a] -> BState Index [a]
elemsByNotIndsM js  = foldrM (\x -> BState . notInds js x) []

-- Reverse of elemsByInds. Choose all indexes, which elements from the
-- specified list have.
indsByElemsM :: (a -> a -> Bool) -> [a] -> [a] -> BState Index [Index]
indsByElemsM eq ks  = foldrM (\x -> BState . indsOfs elem' ks x) []
  where elem' y     = foldr (\x z -> if x `eq` y then True else z) False

-- Unwrap monad from list indexing functions.
elemsByInds :: [a] -> [Index] -> [a]
elemsByInds xs js       = fst $ runBState (elemsByIndsM js xs) indBase

elemsByNotInds :: [a] -> [Index] -> [a]
elemsByNotInds xs js    = fst $ runBState (elemsByNotIndsM js xs) indBase

indsByElems :: (a -> a -> Bool) -> [a] -> [a] -> [Index]
indsByElems eq xs ks    = fst $ runBState (indsByElemsM eq ks xs) indBase

elemByInd :: [a] -> Index -> [a]
elemByInd xs j          = elemsByInds xs [j]

indsByElem :: (a -> a -> Bool) -> [a] -> a -> [Index]
indsByElem eq xs k      = indsByElems eq xs [k]

-- Convert list of elements into list of corresponding indexes in "reference"
-- list (preserving order of elements).
elemsOrder :: (a -> a -> Bool) -> [a] -> [a] -> [Index]
elemsOrder eq xs ks = ks >>= indsByElem eq xs


