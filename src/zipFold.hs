
import Data.Monoid
import Control.Monad
import Control.Monad.State
import SgfList

{-
maybeFoldMap :: (Monoid b) => [a -> b] -> [a] -> Maybe b
maybeFoldMap fs xs  =
    let (y, fs') = runBState (foldrM (\x -> BState . f x) (Just mempty) xs) fs
    in  case fs' of
          [] -> y
          _  -> Nothing
  where
    f _ _ []        = (Nothing, [])
    f x mz (f : fs) = (mz >>= \z -> return (f x `mappend` z), fs)-}

-- Also hangs on infinity list due to final state calculation.
maybeFoldMap :: (Monoid b) => [a -> b] -> [a] -> Maybe b
maybeFoldMap fs xs  = case runBState (maybeFoldMapM xs) fs of
                        (y, []) -> y
                        (_, _)  -> Nothing

maybeFoldMap' :: (Monoid b) => [a -> b] -> [a] -> Maybe b
maybeFoldMap' fs xs  = fst $ runBState (maybeFoldMapM xs) fs

maybeFoldMapM :: (Monoid b) => [a] -> BState [a -> b] (Maybe b)
maybeFoldMapM       = foldrM (\x -> BState . f x) (Just mempty)
  where
    f _ _ []        = (Nothing, [])
    f x mz (f : fs) = (mz >>= \z -> return (f x `mappend` z), fs)

-- FIXME: Use (reverse xs) instead. But in this case to preserve order i need
-- z `mappend` f x in maybeFoldMap1M?
maybeFoldMap1 :: (Monoid b) => [a -> b] -> [a] -> Maybe b
maybeFoldMap1 fs xs = do
    (y, fs) <- runStateT (maybeFoldMap1M xs) (reverse fs)
    case fs of
      [] -> return y
      _  -> Nothing

maybeFoldMap1M :: (Monoid b) => [a] -> StateT [a -> b] Maybe b
maybeFoldMap1M      = foldrM (\x -> StateT . f x) mempty
  where
    f _ _ []        = Nothing
    f x z (f : fs)  = return (f x `mappend` z, fs)

maybeFoldMap2 :: (Monoid b) => [a -> b] -> [a] -> Maybe b
maybeFoldMap2 [] [] = Just mempty
maybeFoldMap2 (f : fs) (x : xs)
                    = liftM2 mappend (Just (f x)) (maybeFoldMap2 fs xs)
maybeFoldMap2 _ _   = Nothing



zipFold3 :: (MonadPlus m, Monoid b) => [a -> m b] -> [a] -> m b
zipFold3 fs xs = do
    (y, fs) <- runStateT (zipFold3M xs) (reverse fs)
    case fs of
      [] -> return y
      _  -> mzero

zipFold3M :: (MonadPlus m, Monoid b) => [a] -> StateT [a -> m b] m b
zipFold3M           = foldrM (\x -> StateT . f3M x) mempty

f3M :: (MonadPlus m, Monoid b) => a -> b -> [a -> m b] -> m (b, [a -> m b])
f3M _ _ []        = mzero
f3M x z (f : fs)  = f x >>= \y -> return (y `mappend` z, fs)

-- Equivalent of zipFold3 with foldr instead of foldrM.
zipFold3' :: (MonadPlus m, Monoid b) => [a -> m b] -> [a] -> m b
zipFold3' fs xs = do
    (y, fs) <- runStateT (zipFold3M' xs) (reverse fs)
    case fs of
      [] -> return y
      _  -> mzero

zipFold3M' :: (MonadPlus m, Monoid b) => [a] -> StateT [a -> m b] m b
zipFold3M'          = foldr (\x mtz -> mtz >>= \z -> StateT (f3M x z)) (return mempty)


zipFold4 :: (MonadPlus m, Monoid b) => [a -> m b] -> [a] -> m b
zipFold4 fs xs =
    case runState (zipFold4M xs) (reverse fs) of
      (y, []) -> y
      _       -> mzero

zipFold4M :: (MonadPlus m, Monoid b) => [a] -> State [a -> m b] (m b)
zipFold4M       = foldrM (\x -> State . f4M x) (return mempty)

f4M :: (MonadPlus m, Monoid b) => a -> m b -> [a -> m b] -> (m b, [a -> m b])
f4M _ _  []        = (mzero, [])
f4M x mz (f : fs)  = (f x `mplus` mz, fs)

zipFold4M' :: (MonadPlus m, Monoid b) => [a] -> State [a -> m b] (m b)
zipFold4M'           = foldr (\x msz -> msz >>= State . (f4M x)) (return (return mempty))

