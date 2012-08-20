
import System.IO                -- For hX
import System.Environment       -- For getArgs
import Data.List                -- For groupBy
import Data.Char                -- For isSpace
import Data.Maybe               -- For fromJust
import Control.Monad
import Control.Monad.State      -- For State monad.

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

-- Data.List does not contain it on my system.
dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd p      = foldr (\x z -> if null z && p x then [] else x : z) []

-- Split list by separator (omitting separator itself).
splitBy :: (Eq a) => a -> [a] -> [[a]]
splitBy s           = foldr go [[]]
  where
    go x zl@(z : zs) = if x == s then [] : zl else (x : z) : zs

-- FIXME: Use isSapce instead of check for ' '.
-- FIXME: Use dropWhile and dropWhileEnd instead of rmSurrSpaces.
-- FIXME: First split list by separator (omitting separator), then parse it
-- with State monad. I.e. group elements at the level lower, then State monads
-- works at.
main                =  do
    (file : args) <- getArgs
    handle <- openFile file ReadMode
    contents <- hGetContents handle
    sequence $ map putStrLn $ lines contents
--getContents >>= sequence . map putStrLn . lines

-- Phrase separator.
phrSep              = '-'
-- Start index.
indBase             = 1

-- Split each string to phrases by phrSep character (omitting separator
-- itself) and remove leading and trailing spaces from each phrase (phrase may
-- contain spaces).
splitToPhrases :: [String] -> [[String]]
splitToPhrases      = map splitToPhrases'
  where
    splitToPhrases' :: String -> [String]
    splitToPhrases' = map dropSpaces . splitBy phrSep
    dropSpaces :: String -> String
    dropSpaces      = dropWhile isSpace . dropWhileEnd isSpace

-- Indexed lists {{{

-- Choose phrase from list by index.
phraseByInd :: [String] -> Index -> [String]
phraseByInd ls i    = fst $ runBState phraseByInd' indBase
  where
    phraseByInd' :: BState Index [String]
    phraseByInd'    = foldrM (\x -> BState . onlyInd i x) [] ls

-- Complement phraseByInd. Choose all phrases from list with index not equal
-- to specified one.
phraseByNotInd :: [String] -> Index -> [String]
phraseByNotInd ls i = fst $ runBState phraseByNotInd' indBase
  where
    phraseByNotInd' :: BState Index [String]
    phraseByNotInd' = foldrM (\x -> BState . notInd i x) [] ls

-- Reverse of phraseByInd. Determine index by phrase.
indByPhrase :: [String] -> String -> Maybe Index
indByPhrase ls y    = fst $ runBState indByPhrase' indBase
  where
    indByPhrase' :: BState Index (Maybe Index)
    indByPhrase'    = foldrM (\x z -> BState $ indOf y x z) Nothing ls


-- Choose word from string by index.
phraseByInd1 :: String -> Index -> String
phraseByInd1 ls i   = fst $ runBState phraseByInd1' indBase
  where
    phraseByInd1' :: BState Index String
    phraseByInd1'   = foldrM (\x -> BState . onlyInd1 phrSep i x) "" ls

-- Choose all other words from string with index not equal to specified one.
phraseByNotInd1 :: String -> Index -> String
phraseByNotInd1 ls i = fst $ runBState phraseByNotInd1' indBase
  where
    phraseByNotInd1' :: BState Index String
    phraseByNotInd1' = foldrM (\x -> BState . notInd1 phrSep i x) "" ls

-- END Indexed lists }}}
-- Folding functions for use in State or Backward State monads. {{{

-- Add list element x to the accumulator z only if its index s equals to i.
onlyInd :: Index -> a -> [a] -> Index -> ([a], Index)
onlyInd i x z       = \s -> let z' = if s == i then x : z else z
                            in  (z', s + 1)

-- Complement to onlyInd. Add each list elements x to accumulator z if its
-- index s not equals to i.
notInd :: Index -> a -> [a] -> Index -> ([a], Index)
notInd i x z        = \s -> let z' = if s /= i then x : z else z
                            in  (z', s + 1)

-- Reverse of onlyInd. Set accumulator to index of list element y. If there is
-- several, first one (closer to the list head) will be used.
indOf :: (Eq a) => a -> a -> Maybe Index -> Index -> (Maybe Index, Index)
indOf y x z         = \s -> let z' = if x == y then Just s else z
                            in  (z', s + 1)


-- Add element x to the accumulator z only if elememt's index s equals to i.
-- Elements are separated by 'sep'.
onlyInd1 :: (Eq a) => a -> Index -> a -> [a] -> Index -> ([a], Index)
onlyInd1 sep i x z  = \s -> if x == sep
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
notInd1 :: (Eq a) => a -> Index -> a -> [a] -> Index -> ([a], Index)
notInd1 sep i x z   = \s ->
                        if x == sep
                          then let s' = s + 1
                                   z' = if s /= i && not (null z)
                                          then sep : z else z
                               in  (z', s')
                          else if s /= i then (x : z, s)  else (z, s)

-- END Several folding functions for use in State or Backward State monads. }}}

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

