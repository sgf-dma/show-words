
module SgfOrderedLine
    ( Index
    , Line (..)
    , orderList
    , onlyOrdered
    , onlyOthers
    , mapOrdered
    , mapOthers
    , zipAppOrdered
    , zipAppOthers
    )
  where

-- FIXME: Do not export Line data contructor!

import Data.Monoid
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import Control.Applicative

import SgfList

-- FIXME: With inifity index list orderList will hang on any attempt to
-- evaluate "other" elements. Though, this may work in some cases. E.g. with
-- (<*>), if f a contains empty "other" part, it may work. For this to work i
-- need to pattern match in zipWith to its 2nd argument first (which will be
-- empty list). To do this, i should zip with (flip ($)). Though, this will
-- hang in reverse case: when function f (a -> b) contains empty "other" part,
-- but value f a contains infinity "other" part (current implementation in
-- contrary will work in that case). Really, i'm not sure whether this should
-- be fixed. And anyway, i don't know fix to this, which does not break the
-- opposite case.

-- Line represents the idea of line split into two groups: "ordered" elements
-- (1st list) and "other" elements (2nd list).  Function orderList is
-- recommended creation method for Line.
data Line a         = Line [a] [a]
  deriving (Eq, Show)
instance Functor Line where
    fmap f (Line xs ys) = Line (map f xs) (map f ys)
    --fmap f          = (f <$>)
-- Applicative instance is zip for Line-s.
instance Applicative Line where
    pure x          = Line (repeat x) (repeat x)
    (Line fs gs) <*> (Line xs ys)
                    = Line (zipWith ($) fs xs) (zipWith ($) gs ys)
--                    = Line (zipWith (flip ($)) xs fs) (zipWith (flip ($)) ys gs)
-- For making list from Line.
instance F.Foldable Line where
    foldMap f (Line xs ys)  = (F.foldMap f xs) `mappend` (F.foldMap f ys)
-- For mapM.
instance T.Traversable Line where
    traverse f (Line xs ys) = Line <$> (T.traverse f xs) <*> (T.traverse f ys)
    --sequenceA (Line xs ys)  = Line <$> T.sequenceA xs <*> T.sequenceA ys
-- For "scoping" functions on Line to either only "ordered" or only "other"
-- elements. Use onlyOrdered and onlyOthers first to split Line into two, then
-- apply function to corresponding one, then merge them back using mappend.
instance Monoid (Line a) where
    mempty          = Line [] []
    (Line xs ys) `mappend` (Line xs' ys')
                    = Line (xs `mappend` xs') (ys `mappend` ys')

-- Convert list to Line by splitting it to "ordered" elements and "other"
-- elements using supplied indexes list (order of "ordered" elements is the
-- same as indexes). Note, that with infinity indexes list only "ordered" part
-- of Line may be defined, and any attempt to evaluate (e.g. by pattern match)
-- "other" part of Line will never terminate.
-- FIXME: elemsByInds should preserve order. In such case i will no longer
-- need bind.
orderList :: [Index] -> [a] -> Line a
orderList order     = Line  <$> (\xs -> order >>= flip elemByInd xs)
                            <*> (order `elemsByNotInds`)

onlyOrdered :: Line a -> Line a
onlyOrdered (Line xs _)    = Line xs []

onlyOthers :: Line a -> Line a
onlyOthers (Line _ ys)     = Line [] ys

-- Some more "specific" instances.
mapOrdered :: (a -> a) -> Line a -> Line a
mapOrdered f        = mappend <$> fmap f . onlyOrdered <*> onlyOthers
--mapOrdered f x      = Line (repeat f) (repeat id) <*> x

mapOthers :: (a -> a) -> Line a -> Line a
mapOthers f         = mappend <$> onlyOrdered <*> fmap f . onlyOthers
--mapOthers f xs      = Line (repeat id) (repeat f) <*> xs

zipAppOrdered :: [a -> a] -> Line a -> Line a
zipAppOrdered fs    = mappend <$> zipApp fs . onlyOrdered <*> onlyOthers

zipAppOthers :: [a -> a] -> Line a -> Line a
zipAppOthers fs     = mappend <$> onlyOrdered <*> zipApp fs . onlyOthers

