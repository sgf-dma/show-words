
import System.IO                -- For hX
import System.Environment       -- For getArgs
import Data.List                -- For groupBy
import Data.Char                -- For isSpace
import Data.Maybe               -- For fromJust
import Data.Monoid
import Control.Monad
import Control.Applicative      -- For Applicative ((->) a).
import Control.Monad.State      -- For State monad.
import Control.Monad.Reader

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

type Index          =  Int
-- Folding functions for use in State or Backward State monads.
-- Add list element x to the accumulator z only if its index s equals to i.
onlyInds :: [Index] -> a -> [a] -> Index -> ([a], Index)
onlyInds js x z     = \s -> let z' = if s `elem` js then x : z else z
                            in  (z', s + 1)

-- Complement to onlyInd. Add each list elements x to accumulator z if its
-- index s not equals to i.
notInds :: [Index] -> a -> [a] -> Index -> ([a], Index)
notInds js x z      = \s -> let z' = if s `notElem` js then x : z else z
                            in  (z', s + 1)

-- FIXME: Supply own equality test. This allows to remove (Eq a) restriction
-- and allow to use some prefix matching instead of `elem`.
-- Reverse of onlyInd. Set accumulator to index of list element y. If there is
-- several, first one (closer to the list head) will be used.
indsOfs :: (Eq a) => [a] -> a -> [Index] -> Index -> ([Index], Index)
indsOfs ys x z      = \s -> let z' = if x `elem` ys then s : z else z
                            in  (z', s + 1)


-- Index list by folding inside Backward State monad.
elemsByInds :: [Index] -> [a] -> BState Index [a]
elemsByInds js      = foldrM (\x -> BState . onlyInds js x) []

elemsByNotInds :: [Index] -> [a] -> BState Index [a]
elemsByNotInds js   = foldrM (\x -> BState . notInds js x) []

indsByElems :: (Eq a) => [a] -> [a] -> BState Index [Index]
indsByElems ys      = foldrM (\x -> BState . indsOfs ys x) []

-- END Indexed list. }}}

-- Phrase separator.
phrSep              = '-'
-- Start index.
indBase             = 1

-- Choose phrase from list by index.
phraseByInd :: [String] -> Index -> [String]
phraseByInd ls i    = fst $ runBState (elemsByInds [i] ls) indBase

-- Complement phraseByInd. Choose all phrases from list with index not equal
-- to specified one.
phrasesByNotInds :: [String] -> [Index] -> [String]
phrasesByNotInds ls js = fst $ runBState (elemsByNotInds js ls) indBase

-- Reverse of phraseByInd. Determine index by phrase.
indsByPhrase :: [String] -> String -> [Index]
indsByPhrase ls y   = fst $ runBState (indsByElems [y] ls) indBase

-- Split each string to phrases by phrSep character (omitting separator
-- itself) and remove leading and trailing spaces from each phrase (phrase may
-- contain spaces).
splitToPhrases :: String -> [String]
splitToPhrases      = map dropSpaces . splitBy phrSep
  where
    dropSpaces :: String -> String
    dropSpaces      = dropWhile isSpace . dropWhileEnd isSpace

reorderPhrases :: [String] -> [String] -> [[String]]
reorderPhrases colNames (refs : ls)
                    = map (orderLine . splitToPhrases) ls
  where
    phrOrder :: [Index]
    phrOrder        = colNames >>= indsByPhrase refs'
      where refs'   = splitToPhrases refs
    orderLine :: [String] -> [String]
    orderLine       = (++) <$> orderedPhrases <*> otherPhrases
      where
        orderedPhrases ps   = phrOrder >>= phraseByInd ps
        otherPhrases ps     = phrasesByNotInds ps phrOrder

main                =  do
    (file : args) <- getArgs
    handle <- openFile file ReadMode
    contents <- hGetContents handle
    sequence $ map putStrLn $ concat $ reorderPhrases args $ lines contents
    hClose handle

