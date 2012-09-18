
import Control.Monad.State
import Control.Monad.Reader

foldrM :: (Monad m) => (a -> b -> m b) -> b -> [a] -> m b
foldrM _ z []         = return z
foldrM g z (x : xs)   = foldrM g z xs >>= g x
newtype BState s a  = BState {runBState :: (s -> (a, s))}
instance Monad (BState s) where
    return x        = BState (\s -> (x, s))
    BState m >>= f  = BState $ \s2 ->
                        let (x, s0)   = m s1
                            BState m' = f x
                            (x', s1)  = m' s2
                        in  (x', s0)
type Index          = Int
indBase             = 1

-- 2nd function series: State inside StateT in two combinations. Both of them
-- work.
onlyInds2 :: a -> [a] -> Index -> [Index] -> (([a], Index), [Index])
onlyInds2 x zs s js
  | s `elem` js     = ((x : zs, s + 1), filter (/= s) js)
  | otherwise       = ((zs, s + 1), js)

onlyInds2' :: a -> [a] -> [Index] -> Index -> (([a], [Index]), Index)
onlyInds2' x zs js s
  | s `elem` js     = ((x : zs, filter (/= s) js), s + 1)
  | otherwise       = ((zs, js), s + 1)

elemsByIndsM2 :: [a] -> StateT Index (State [Index]) [a]
elemsByIndsM2       = foldrM f []
  where
    f :: a -> [a] -> StateT Index (State [Index]) [a]
    f x zs          = StateT $ \s -> State $ onlyInds2 x zs s

elemsByIndsM2' :: [a] -> StateT [Index] (State Index) [a]
elemsByIndsM2'      = foldrM f []
  where
    f :: a -> [a] -> StateT [Index] (State Index) [a]
    f x zs          = StateT $ \js -> State $ onlyInds2' x zs js

elemsByInds2 :: [Index] -> [a] -> (([a], Index), [Index])
elemsByInds2 js xs  = runState (runStateT (elemsByIndsM2 xs) indBase) js

elemsByInds2' :: [Index] -> [a] -> (([a], [Index]), Index)
elemsByInds2' js xs = runState (runStateT (elemsByIndsM2' xs) js) indBase


newtype StateT' s m a   = StateT' {runStateT' :: s -> m (a, s)}
instance (Monad m) => Monad (StateT' s m) where
    return x        = StateT' $ \s -> return (x, s)
    mtx >>= f       = StateT' $ \s0 ->
                        runStateT' mtx s0 >>= \ (x, s1) ->
                        runStateT' (f x) s1

onlyInds3 :: a -> [a] -> [Index] -> Index -> (([a], [Index]), Index)
onlyInds3 x zs js s = ( if s `elem` js
                          then (x : zs, filter (/= s) js)
                          else (zs, js)
                      , s + 1)

elemsByIndsM3 :: [a] -> StateT' [Index] (BState Index) [a]
elemsByIndsM3       = foldrM f []
  where
    f :: a -> [a] -> StateT' [Index] (BState Index) [a]
    f x zs          = StateT' $ \js -> BState $ onlyInds3 x zs js

elemsByInds3 :: [Index] -> [a] -> (([a], [Index]), Index)
elemsByInds3 js xs  = runBState (runStateT' (elemsByIndsM3 xs) js) indBase


onlyInds3' :: a -> [a] -> Index -> Index -> ([a], Index)
onlyInds3' x zs j s = ( if s == j
                          then x : zs
                          else zs
                      , s + 1)

elemsByIndsM3' :: Index -> [a] -> BState Index [a]
elemsByIndsM3' j    = foldrM f []
  where
    f :: a -> [a] -> BState Index [a]
    f x zs          = BState $ onlyInds3' x zs j

elemsByInds3' :: Index -> [a] -> ([a], Index)
elemsByInds3' j xs  = runBState (elemsByIndsM3' j xs) indBase


-- 3rd function series is an attempt to wrap BState inside StateT transformer.
-- This does not work, though. And i can't say for sure why. Evaluation order
-- is not exactly that i imagine (use single-step execution from ghci), but
-- anyway, even if this will not hang, it is useless, because StateT is State
-- after all and is _external_ monad.  In other words, foldrM calls evaluation
-- of StateT's bind, but its bind passes initial state to the first monad in
-- the bind chain (i.e. requires the rest of the bind chain to be evaluted,
-- the same as with State). So, initial js value will be at the _last_ element
-- of the list, though, initial index (counted by BState) will be (in theory)
-- at the first of element of the list. So, this is a bit contradictory.  More
-- about evaluation: BState does not computes its state during StateT
-- evaluation, until i reach the beginning of the bind chain. Then BState
-- state is required by return in StateT's return. And here it hangs and fails
-- to compute it (to not hang it should evaluate state along all passed bind
-- chain). I'm not sure, where break really is, but obviously, StateT state
-- computing direction is wrong and even if function will work, it will not
-- work on infinity lists as other State functions. In other words, to
-- implement this with transformers, i should use BState transformer with
-- BState monad. And then it may be work..




onlyInds :: (Index -> Index -> Bool)
         -> a -> [a] -> (Index, [Index]) -> ([a], (Index, [Index]))
onlyInds _  _ _  (s, [])
                    = ([], (s + 1, []))
onlyInds eq x zs (s, js)
  | any (s `eq`) js = (x : zs, (s + 1, filter (not . (s `eq`)) js))
  | otherwise       = (zs, (s + 1, js))

onlyElems :: (a -> a -> Bool)
          -> a -> [Index] -> (Index, [a]) -> ([Index], (Index, [a]))
onlyElems _  _ _  (s, []) = ([], (s + 1, []))
onlyElems eq x zs (s, ks)
  | any (x `eq`) ks = (s : zs, (s + 1, filter (not . (x `eq`)) ks))
  | otherwise       = (zs, (s + 1, ks))


f :: (a -> a -> Bool) -> a -> [a] -> b -> [b] -> ([b], [a])
f _  _ [] _ _       = ([], [])
f eq x ks z zs
  | any (x `eq`) ks = (z : zs, filter (not . (x `eq`)) ks)
  | otherwise       = (zs, ks)

onlyInds' :: (Index -> Index -> Bool)
         -> a -> [a] -> (Index, [Index]) -> ([a], (Index, [Index]))
onlyInds' eq x zs (s, js) = let (zs', js') = f eq s js x zs
                            in  (zs', (s + 1, js'))

onlyElems' :: (a -> a -> Bool)
           -> a -> [Index] -> (Index, [a]) -> ([Index], (Index, [a]))
onlyElems' eq x zs (s, ks)  = let (zs', ks') = f eq x ks s zs
                              in  (zs', (s + 1, ks'))




-- This function does need fixity point, because it is generally not finite.
f0 :: (a -> Bool) -> [a] -> b -> [b] -> ([b], [a])
f0 p ks x zs
  | any p ks        = (x : zs, ks)
  | otherwise       = (zs, ks)

-- If at least one element from key list satisfies predicate, add x to result
-- and remove matched element from key list. Empty key list is fixity point.
f1 :: (a -> Bool) -> [a] -> b -> [b] -> ([b], [a])
f1 _ [] _ _         = ([], [])
f1 p ks x zs
  | any p ks        = (x : zs, filter (not . p) ks)
  | otherwise       = (zs, ks)

onlyInds'' :: (Index -> Index -> Bool)
         -> a -> [a] -> (Index, [Index]) -> ([a], (Index, [Index]))
onlyInds'' eq x zs (s, js) = fmap ((,) (s + 1)) $ f1 (s `eq`) js x zs

onlyElems'' :: (a -> a -> Bool)
           -> a -> [Index] -> (Index, [a]) -> ([Index], (Index, [a]))
onlyElems'' eq x zs (s, ks) = fmap ((,) (s + 1)) $ f1 (x `eq`) ks s zs


f2 :: (Monad m) => a -> [a] -> b -> [b] -> ReaderT (a -> a -> Bool) m ([b], [a])
f2 _ [] _ _         = return ([], [])
f2 x ks z zs        = do
                        eq <- ask
                        if any (x `eq`) ks
                          then return (z : zs, filter (not . (x `eq`)) ks)
                          else return (zs, ks)

