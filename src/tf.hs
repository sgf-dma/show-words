{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

import Data.List (deleteFirstsBy)
import Data.Monoid
import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Reader
import Control.Applicative
import qualified Data.Foldable as F

newtype StateT' s m a   = StateT' {runStateT' :: s -> m (a, s)}
instance (Monad m) => Monad (StateT' s m) where
    return x        = StateT' $ \s -> return (x, s)
    mtx >>= f       = StateT' $ \s0 -> do
                        (x, s1) <- runStateT' mtx s0
                        runStateT' (f x) s1
instance MonadTrans (StateT' s) where
    lift mx         = StateT' $ \s -> do
                        x <- mx
                        return (x, s)
instance (Monad m) => MonadState s (StateT' s m) where
    get             = StateT' $ \s -> return (s, s)
    put s'          = StateT' $ \_ -> return ((), s')

newtype Reader' r a = Reader' {runReader' :: r -> a}
instance Monad (Reader' r) where
    return x        = Reader' $ \_ -> x
    mx >>= f        = Reader' $ \r -> 
                        let g = runReader' mx
                            mx' = f (g r)
                        in  runReader' mx' r

newtype ReaderT' r m a  = ReaderT' {runReaderT' :: r -> m a}
instance (Monad m) => Monad (ReaderT' r m) where
    return x        = ReaderT' $ \_ -> return x
    mtx >>= f       = ReaderT' $ \r -> do
                        x <- runReaderT' mtx r
                        runReaderT' (f x) r
instance MonadTrans (ReaderT' r) where
    lift mx         = ReaderT' $ \_ -> mx
instance (Monad m) => MonadReader r (ReaderT' r m) where
    ask             = ReaderT' $ return
    local f mtx     = ReaderT' $ runReaderT' mtx . f

f :: Eq a => a -> [[a]] -> ReaderT [a] (State [a]) [[a]]
f x (z : zs)        = do
    sp <- ask
    case sp of
      []       -> return $ (x : z) : zs
      (k : ks) -> do
        csp <- lift get
        case csp of
          []
            | x == k -> do
              lift . put $ ks
              return matched_x
            | otherwise -> do
              lift . put $ (k : ks)
              return matched_x
            where
              matched_x = [x] : deleteFirstsBy (==) z sp : zs
          (c : cs) -> do
            lift . put $
              if x == c        then cs
                else if x == k then ks
                               else k : ks
            return $ (x : z) : zs

f1 :: Eq a => a -> [[a]] -> ReaderT [a] (State [a]) [[a]]
f1 x (z : zs)        = do
    sp <- ask
    cs <- lift get
    lift . put $ newSt sp cs
    return $ newAcc sp cs
  where
    --newSt :: [a] -> [a] -> [a]
    newSt [] _      = []
    newSt _ (c : cs)
      | x == c      = cs
    newSt (k : ks) _
      | x == k      = ks
      | otherwise   = (k : ks)
    --newAcc :: [a] -> [a] -> [[a]]
    newAcc sp@(_ : _) [] = [x] : deleteFirstsBy (==) z sp : zs
    newAcc _  _          = (x : z) : zs

f2 :: Eq a => a -> [[a]] -> ReaderT [a] (State [a]) [[a]]
f2 x (z : zs)        = do
    sp <- ask
    cs <- lift get
    newSt sp cs
  where
    --newSt :: [a] -> [a] -> ReaderT [a] (State [a]) [[a]]
    newSt [] _      = do
                        lift . put $ []
                        return add_x
    newSt _ (c : cs)
      | x == c      = do
                        lift . put $ cs
                        return add_x
    newSt (k : ks) cs
      | x == k      = do
                        lift . put $ ks
                        return (maybe_add_x cs)
      | otherwise   = do
                        lift . put $ (k : ks)
                        return (maybe_add_x cs)
      where
        maybe_add_x []  = [x] : deleteFirstsBy (==) z (k : ks) : zs
        maybe_add_x _   = add_x
    add_x        = (x : z) : zs

f3 :: (a -> a -> Bool) -> a -> [[a]] -> ReaderT [a] (State [a]) [[a]]
f3 eq x zs    = do
    sp <- ask
    cs <- lift get
    let (zs', cs') = doStep sp cs
    lift . put $ cs'
    return zs'
  where
    --doStep :: [a] -> [a] -> ReaderT [a] (State [a]) [[a]]
    doStep [] _     = (add_x zs, [])
    doStep _ (c : cs)
      | x `eq` c    = (add_x zs, cs)
    doStep (k : ks) cs
      | x `eq` k    = ((add_x . maybe_new cs) zs, ks)
      | otherwise   = ((add_x . maybe_new cs) zs, k : ks)
      where
        maybe_new [] (z : zs) = [] : deleteFirstsBy eq z (k : ks) : zs
    add_x []        = [x] : zs
    add_x (z : zs)  = (x : z) : zs

f4 :: (a -> a -> Bool) -> a -> [[a]] -> ReaderT [a] (State [a]) [[a]]
f4 eq x (z1 : z2 : zs)  = do
    sp <- ask
    cs <- lift get
    let (zs', cs') = doStep sp cs
    lift . put $ cs'
    return zs'
  where
    --doStep :: [a] -> [a] -> ReaderT [a] (State [a]) [t a]
    doStep [] _     = ([] : (x : z2) : zs, [])
    doStep ks [c]
      | x `eq` c    = ([] : [] : z2 : zs, ks)
    doStep (k : ks) (c : cs)
      | x `eq` c    = ((x : z1) : z2 : zs, cs)
      | x `eq` k    = ([x] : (z1 ++ z2) : zs, ks)
      | otherwise   = ([]  : (x : z1 ++ z2) : zs, k : ks)

f4M :: (Alternative t) => (a -> a -> Bool) -> a -> [t a] -> ReaderT [a] (State [a]) [t a]
f4M eq x (z1 : z2 : zs)  = do
    sp <- ask
    cs <- lift get
    let (zs', cs') = f sp cs
    lift . put $ cs'
    return zs'
  where
    --f :: [a] -> [a] -> ReaderT [a] (State [a]) [t a]
    f [] _          = (empty : (pure x <|> z2) : zs, [])
    f ks [c]
      | x `eq` c    = (empty : empty : z2 : zs, ks)
    f (k : ks) (c : cs)
      | x `eq` c    = ((pure x <|> z1) : z2 : zs, cs)
      | x `eq` k    = (pure x : (z1 <|> z2) : zs, ks)
      | otherwise   = (empty  : (pure x <|> z1 <|> z2) : zs, k : ks)

