
import System.IO
import System.Environment
import Control.Monad
import Control.Monad.State

type Index          =  Int
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
foldlM              :: (Monad m) => (b -> a -> m b) -> b -> [a] -> m b
foldlM g z []       =  return z
foldlM g z (x : xs) =  g z x >>= \z' -> foldlM g z' xs

main                =  do
    (file : args) <- getArgs
    handle <- openFile file ReadMode
    contents <- hGetContents handle
    sequence $ map putStrLn $ lines contents
--getContents >>= sequence . map putStrLn . lines

--fieldNameToNum
seqElems :: String -> [String]
seqElems ls         =  map (elemByInd ls) [2,1,3]

    --map (elemByInd ls) transp

-- Start index.
indBase             = 1
-- Word separator.
wordSep             = '-'

-- Select element (separated by dash) with specific index from list.  Element
-- index counts from 1.
elemByInd :: String -> Int -> String
elemByInd ls i      =  fst $ runBState (foldrM elemByInd' "" ls) indBase
  where
    -- Increase state at '-' separator. Output element (all between
    -- separators) with index i.
    -- FIXME: Use Reader monad? Instead of checking state?
    elemByInd' :: Char -> String -> BState Int String
    elemByInd' x z  =  BState $ \s ->
                        if x == '-'
                          then (z, s + 1)
                          else if s == i then (x : z, s) else (z, s)


-- Select element (separated by dash) with specific index from list.  Element
-- index counts from 1.
elemByIndNot :: String -> Int -> String
elemByIndNot ls i   =  fst $ runBState (foldrM elemByInd' "" ls) indBase
  where
    -- Increase state at '-' separator. Output element (all between
    -- separators) with index i.
    -- FIXME: Use Reader monad? Instead of checking state?
    elemByInd' :: Char -> String -> BState Int String
    elemByInd' x z  =  BState $ \s ->
                        if x == '-'
                          then let s' = s + 1
                               in  if s' /= i then ('-' : z, s') else (z, s')
                          else     if s  /= i then (x   : z, s)  else (z, s)


-- Choose word from string by index.
wordByInd :: String -> Index -> String
wordByInd ls i      = fst $ runBState wordByInd' indBase
  where
    wordByInd' :: BState Index String
    wordByInd'      = foldrM (\x -> BState . onlyInd wordSep i x) "" ls

-- Choose all other words with index not equal to specified one from string.
wordByNotInd :: String -> Index -> String
wordByNotInd ls i   = fst $ runBState wordByNotInd' indBase
  where
    wordByNotInd' :: BState Index String
    wordByNotInd'   = foldrM (\x -> BState . notInd wordSep i x) "" ls

wordByNotInd1 :: String -> Index -> String
wordByNotInd1 ls i  = fst $ runState wordByNotInd' indBase
  where
    wordByNotInd' :: State Index String
    wordByNotInd'   = foldlM (\z x -> State $ notInd wordSep i x z) "" ls


-- Here are several folding functions for use in State or Backward State
-- monads.

-- Add element x to the accumulator z only if elememt's index s equals to i.
-- Elements are separated by 'sep'.
onlyInd :: (Eq a) => a -> Index -> a -> [a] -> Index -> ([a], Index)
onlyInd sep i x z   = \s -> if x == sep
                              then (z, s + 1)
                              else if s == i then (x : z, s) else (z, s)
-- Add element x to the accumulator z only if element's index s does _not_
-- equal to i. Elements are seprated by sep. Several elements in the result
-- is possible.
-- Check 'not (null z)' requires remaining bind chain to be computed, so it
-- should not block new state computation, otherwise Backward State monad will
-- hang.
-- I should decide to add separator by checking previous state, but not next
-- one. Otherwise, check 'not (null z)' will be always true (it will not with
-- left fold, but left fold reverses list).
notInd :: (Eq a) => a -> Index -> a -> [a] -> Index -> ([a], Index)
notInd sep i x z    = \s ->
                        if x == sep
                          then let s' = s + 1
                                   z' = if s /= i && not (null z)
                                          then sep : z else z
                               in  (z', s')
                          else if s /= i then (x : z, s)  else (z, s)

{-
notInd :: (Eq a) => a -> Index -> a -> [a] -> Index -> ([a], Index)
notInd sep i x z    = \s ->
                        if x == sep
                          then let s' = s + 1
                               in  if s' /= i then (sep : z, s') else (z, s')
                          else     if s  /= i then (x   : z, s)  else (z, s)
-}


-- Remove leading and trailing spaces.
rmSurrSpaces :: String -> String
rmSurrSpaces ls     =  fst $ runBState (fst $ runState  (withoutSpaces ls)
                                                        indBase)
                                       indBase
  where
    -- This is function for use with State or BState wrapper, which skips all
    -- list elements xI from monad result (fold accumulator) until monad state
    -- is initial State.
    skipInitSt :: Char -> String -> Int -> (String, Int)
    skipInitSt x z  =  \s ->
                        if s == indBase
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

