
import Control.Monad.State

newtype BState s a  =  BState {runBState :: (s -> (a, s))}
-- From "The essence of functional programming" by Philip Wadler.
instance Monad (BState s) where
    return x        =  BState (\s -> (x, s))
    BState m >>= f  =  BState $ \s2 ->
                        let (x, s0)   = m s1
                            BState m' = f x
                            (x', s1)  = m' s2
                        in  (x', s0)

foldrM :: (Monad m) => (a -> b -> m b) -> b -> [a] -> m b
foldrM g z []       =  return z
foldrM g z (x : xs) =  foldrM g z xs >>= g x
{-
foldlM              :: (Monad m) => (b -> a -> m b) -> b -> [a] -> m b
foldlM g z []       =  return z
foldlM g z (x : xs) =  g z x >>= \z' -> foldlM g z' xs-}

-- Select from list element (separated by dash) with specific index.  Element
-- index counts from 1.
selectElem :: String -> Int -> String
selectElem ls i     =  fst $ runBState (foldrM (elemByInd i) "" ls) 1
  where
    -- Increase state at '-' separator. Output element (all between
    -- separators) with index i.
    -- FIXME: Use Reader monad? Instead of checking state?
    elemByInd :: Int -> Char -> String -> BState Int String
    elemByInd i x z =  BState $ \s ->
                        if x == '-'
                          then (z, s + 1)
                          else if s == i
                                 then (x : z, s)
                                 else (    z, s)

-- Remove leading and trailing spaces.
rmSurrSpaces :: String -> String
rmSurrSpaces ls     =  fst $ runBState (fst $ runState (withoutSpaces ls) 0) 0
  where
    -- Initial state for State and BState monads.
    initSt          =  0
    -- This is function for use with State or BState wrapper, which skips all
    -- list elements xI from monad result (fold accumulator) until monad state
    -- is initial State.
    skipInitSt :: Char -> String -> Int -> (String, Int)
    skipInitSt x z  =  \s ->
                        if s == initSt
                          then if x == ' '
                                 then   (z, s)
                                 else   (x : z, s + 1)
                          else          (x : z, s)
    -- First use State monad to skip trailing spaces, then use Backward
    -- State monad to skip leading spaces from the list.
    withoutSpaces :: String -> State Int (BState Int String)
    withoutSpaces ls = foldrM (\x -> State . skipInitSt x) "" ls >>=
                       return . foldrM (\x -> BState . skipInitSt x) ""

{-
-- This variant includes _following_ dash in result, because state is still i
-- on the following dash as well. E.g. (first state is 1)
--      "ab-cd-.."
-- then x1='a', x2='b', x3='-' - state is 1, but after x3 it will be 2.
selectElem1 :: Int -> (String, Int)
selectElem1 i       =
    runBState   (foldrM (\x z ->
                            BState (\s ->
                                (if s == i   then x : z   else z
                                ,if x == '-' then (s + 1) else s)
                            )
                        )
                        ""
                        "ab-cd-ef"
                ) 1

--rmSurrSpaces1 :: String -> String
rmSurrSpaces1 ls    =  runBState (foldrM elemByInd "" ls) 0
  where
    elemByInd :: Char -> String -> BState Int String
    elemByInd x z
                    =  BState $ \s ->
                        if s == 0
                          then if x == ' '
                                 then   (z, s)
                                 else   (x : z, s + 1)
                          else          (x : z, s)

--rmSurrSpaces2 :: String -> String
rmSurrSpaces2 ls    =  runState (foldrM elemByInd "" ls) 0
  where
    elemByInd :: Char -> String -> State Int String
    elemByInd x z
                    =  State $ \s ->
                        if s == 0
                          then if x == ' '
                                 then   (z, s)
                                 else   (x : z, s + 1)
                          else          (x : z, s)
-}

