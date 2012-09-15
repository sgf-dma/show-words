
module SgfOrderedLine
    ( Line (..)
    , orderList
    , mapLine
    , mapLine1
    , joinLine
    , joinLineM)
  where

-- FIXME: Do not export Line data contructor!

import Control.Applicative

import SgfList

-- Line represents idea of line divided into ordered elements (e.g. by
-- elemsOrder) and other elements (remaining after ordering, i.e. not
-- mentioned in index list for elemsOrder).
-- First list contains ordered elements, second - other elements.
data Line a         = Line [a] [a]
  deriving (Eq, Show)
instance Functor Line where
    fmap            = mapLine1

-- Convert list to Line by splitting to list of ordered elements and list of
-- other elements.
orderList :: [Index] -> [a] -> Line a
orderList order     = Line  <$> (\xs -> order >>= flip elemByInd xs)
                            <*> (order `elemsByNotInds`)

-- map function f over ordered elements and function g over other elements.
mapLine :: (a -> b) -> (a -> b) -> Line a -> Line b
mapLine f g (Line xs ys)   = Line (map f xs) (map g ys)

mapLine1 :: (a -> b) -> Line a -> Line b
mapLine1 f           = mapLine f f

-- Convert Line to list and apply some functions to some elements.
-- Function f applied to all ordered elements, except first (e.g. waiting for
-- a key before outputting next element). 
-- Function g is "joining" function. It is applied to all elements (both
-- ordered and others), except first (e.g. prepend spaces to strings to make
-- resulted list suitable for concat).
joinLine :: (a -> a) -> (a -> a) -> Line a -> [a]
joinLine _ _ (Line [] [])       = []
joinLine _ g (Line [] (y : ys)) = y : map g ys
joinLine f g (Line (x : xs) ys) = x : (map (g . f) xs ++ map g ys)

joinLineM :: Monad m => (a -> m a) -> (a -> m a) -> Line a -> [m a]
joinLineM f g       = joinLine (>>= f) (>>= g) . mapLine1 return

