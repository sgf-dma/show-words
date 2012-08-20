
import System.IO                -- For hX
import System.Environment       -- For getArgs
import Data.List                -- For groupBy
import Data.Char                -- For isSpace
import Data.Maybe               -- For fromJust
import Control.Monad
import Control.Applicative
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

-- Indexed list. {{{

-- Folding functions for use in State or Backward State monads.
-- Add list element x to the accumulator z only if its index s equals to i.
onlyInd :: Index -> a -> Maybe a -> Index -> (Maybe a, Index)
onlyInd i x z       = \s -> let z' = if s == i then Just x else z
                            in  (z', s + 1)

-- Complement to onlyInd. Add each list elements x to accumulator z if its
-- index s not equals to i.
notInd :: Index -> a -> [a] -> Index -> ([a], Index)
notInd i x z        = \s -> let z' = if s /= i then x : z else z
                            in  (z', s + 1)

-- Reverse of onlyInd. Set accumulator to index of list element y. If there is
-- several, first one (closer to the list head) will be used.
indsOf :: (Eq a) => a -> a -> [Index] -> Index -> ([Index], Index)
indsOf y x z         = \s -> let z' = if x == y then s : z else z
                            in  (z', s + 1)

-- Index list by folding inside Backward State monads.
elemByInd :: Index -> [a] -> BState Index (Maybe a)
elemByInd i         = foldrM (\x -> BState . onlyInd i x) Nothing

elemsByNotInd :: Index -> [a] -> BState Index [a]
elemsByNotInd i     = foldrM (\x -> BState . notInd i x) []

indsByElem :: (Eq a) => a -> [a] -> BState Index [Index]
indsByElem y        = foldrM (\x -> BState . indsOf y x) []

-- END Indexed list. }}}

-- Phrase separator.
phrSep              = '-'
-- Start index.
indBase             = 1

-- Choose phrase from list by index.
phraseByInd :: [String] -> Index -> Maybe String
phraseByInd ls i    = fst $ runBState (elemByInd i ls) indBase

-- Complement phraseByInd. Choose all phrases from list with index not equal
-- to specified one.
phrasesByNotInd :: [String] -> Index -> [String]
phrasesByNotInd ls i = fst $ runBState (elemsByNotInd i ls) indBase

-- Reverse of phraseByInd. Determine index by phrase.
indsByPhrase :: [String] -> String -> [Index]
indsByPhrase ls y   = fst $ runBState (indsByElem y ls) indBase

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

{-
createOrder :: [String] -> [String] -> [Maybe Index]
createOrder refs    = map (indByPhrase refs)-}

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

