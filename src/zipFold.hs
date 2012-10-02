{-# LANGUAGE TupleSections #-}

import Data.Maybe
import Data.Monoid
import Control.Monad
import Control.Monad.State
import SgfList


-- Fold in monad (BState) with result (m b), where b is monoid and monad m is
-- MonadPlus. Because i have access to (m b) value, i can fold it into single
-- (m b) value using two ways: using mappend in monad m or using mplus.  Note,
-- that i need m to be MonadPlus, because i need mzero (failed context), when
-- lists have different length. Also, i need b to be Monoid, when two lists
-- are empty (this is not fail (mzero) and i need some value to return).
--
-- Fold using mappend in monad m. Note, that this version will work only with
-- infinity state list [a -> m b]. Infinity list [a] will not work, because i
-- pattern match on final state, which is initial state of first monad in bind
-- chain (hence, will never be computed).
zipFoldM :: (MonadPlus m, Monoid b) => [a -> m b] -> [a] -> m b
zipFoldM fs xs      =
    case runBState (zipFoldBS xs) fs of
      (y, []) -> y
      _       -> mzero

zipFoldBS :: (MonadPlus m, Monoid b) => [a] -> BState [a -> m b] (m b)
zipFoldBS           = foldrM (\x -> BState . fM x) (return mempty)

fM :: (MonadPlus m, Monoid b) => a -> m b -> [a -> m b] -> (m b, [a -> m b])
fM _ _  []          = (mzero, [])
fM x mz (f : fs)    = (f x >>= \y -> mz >>= \z -> return (y `mappend` z), fs)

-- Fold using mplus.  Note, that this version works incorrectly with default
-- Maybe's MonadPlus instance:
--
--      > listEq zipFoldM1  [1..3] [1,4,3]
--      True
--
-- but should be False.
zipFoldM1 :: (MonadPlus m, Monoid b) => [a -> m b] -> [a] -> m b
zipFoldM1 fs xs     =
    case runBState (zipFoldBS1 xs) fs of
      (y, []) -> y
      _       -> mzero

zipFoldBS1 :: (MonadPlus m, Monoid b) => [a] -> BState [a -> m b] (m b)
zipFoldBS1          = foldrM (\x -> BState . fM1 x) (return mempty)

fM1 :: (MonadPlus m, Monoid b) => a -> m b -> [a -> m b] -> (m b, [a -> m b])
fM1 _ _  []         = (mzero, [])
fM1 x mz (f : fs)   = (f x `mplus` mz, fs)

-- Fold in monad transformer (StateT s m) with result b, where b is monoid and
-- monad m is MonadPlus. Transformer does not allow to access (m b) value,
-- hence, i can't use mplus for folding result into single (m b) value. So,
-- i'll use mappend in transformer.
--
-- Note, that this version does not work with either infinity list. Infinity
-- state list ([a -> m b]) will not work, because i need to reverse it (to
-- preserve lists elements correspondence).  Infinity [a] list will not work,
-- because State monad assignes initial state to first monad in bind chain,
-- which with foldr corresponds to last element in [a] list. Hence, with
-- infinity [a] list i can't compute _any_ state at all.
zipFoldM2 :: (MonadPlus m, Monoid b) => [a -> m b] -> [a] -> m b
zipFoldM2 fs xs     = do
    (y, fs) <- runStateT (zipFoldST xs) (reverse fs)
    case fs of
      [] -> return y
      _  -> mzero

zipFoldST :: (MonadPlus m, Monoid b) => [a] -> StateT [a -> m b] m b
zipFoldST           = foldrM (\x -> StateT . f x) mempty

f :: (MonadPlus m, Monoid b) => a -> b -> [a -> m b] -> m (b, [a -> m b])
f _ _ []            = mzero
f x z (f : fs)      = f x >>= \y -> return (y `mappend` z, fs)

-- This is StateT version with foldl and, surprisingly, it works on _either_
-- infinity list! This is because of mzero: foldl starts evaluation from the
-- first monad in bind chain (it corresponds to first element in list [a]),
-- and it has all information to compute it immediately (State monad passes
-- initial state to first monad in the chain, remember?). Then, when at some
-- point further due to empty state result becomes mzero, final result will
-- always be mzero and final state is unkown (because mzero does not contain
-- any value (normally, final state of StateT transformer is in monad m's
-- result, remember?)).
zipFoldM2' :: (MonadPlus m, Monoid b) => [a -> m b] -> [a] -> m b
zipFoldM2' fs xs    = do
    (y, fs) <- runStateT (zipFoldST' xs) fs
    case fs of
      [] -> return y
      _  -> mzero

zipFoldST' :: (MonadPlus m, Monoid b) => [a] -> StateT [a -> m b] m b
zipFoldST'          = foldlM (\z x -> StateT (f' x z)) mempty

f' :: (MonadPlus m, Monoid b) => a -> b -> [a -> m b] -> m (b, [a -> m b])
f' _ _ []           = mzero
f' x z (f : fs)     = f x >>= \y -> return (z `mappend` y, fs)

-- Non-monadic version. It works on either infinity list.
zipFoldM3 :: (MonadPlus m, Monoid b) => [a -> m b] -> [a] -> m b
zipFoldM3 [] []     = return mempty
zipFoldM3 (f : fs) (x : xs)
                    = liftM2 mappend (f x) (zipFoldM3 fs xs)
zipFoldM3 _ _       = mzero


listEq :: (Eq a) =>
          ([a -> Maybe All] -> [a] -> Maybe All) -> [a] -> [a] -> Bool
listEq f xs         = let xs' = map (\x -> Just . All . (x ==)) xs
                      in  getAll . fromMaybe (All False) . f xs'

test1 f xs          = let xs' = map (\x -> Just . (: []) . (x, )) xs
                      in  f xs'
