
import System.IO                -- For hX
import System.Environment       -- For getArgs
import Data.List                -- For groupBy
import Data.Char                -- For isSpace
import Data.Maybe               -- For fromJust
import Data.Monoid
import Control.Monad
import Control.Applicative
import Control.Monad.State      -- For State monad.
import Control.Monad.Reader

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
onlyInds :: [Index] -> a -> [a] -> Index -> ([a], Index)
onlyInds js x z     = \s -> let z' = if s `elem` js then x : z else z
                            in  (z', s + 1)

-- Complement to onlyInd. Add each list elements x to accumulator z if its
-- index s not equals to i.
notInd :: Index -> a -> [a] -> Index -> ([a], Index)
notInd i x z        = \s -> let z' = if s /= i then x : z else z
                            in  (z', s + 1)
notInds :: [Index] -> a -> [a] -> Index -> ([a], Index)
notInds js x z      = \s -> let z' = if s `notElem` js then x : z else z
                            in  (z', s + 1)

-- Reverse of onlyInd. Set accumulator to index of list element y. If there is
-- several, first one (closer to the list head) will be used.
indsOf :: (Eq a) => a -> a -> [Index] -> Index -> ([Index], Index)
indsOf y x z         = \s -> let z' = if x == y then s : z else z
                            in  (z', s + 1)
indsOfs :: (Eq a) => [a] -> a -> [Index] -> Index -> ([Index], Index)
indsOfs ys x z      = \s -> let z' = if x `elem` ys then s : z else z
                            in  (z', s + 1)


-- Index list by folding inside Backward State monads.
elemByInd :: Index -> [a] -> BState Index (Maybe a)
elemByInd i         = foldrM (\x -> BState . onlyInd i x) Nothing
elemsByInds :: [Index] -> [a] -> BState Index [a]
elemsByInds js      = foldrM (\x -> BState . onlyInds js x) []

elemsByNotInd :: Index -> [a] -> BState Index [a]
elemsByNotInd i     = foldrM (\x -> BState . notInd i x) []
elemsByNotInds :: [Index] -> [a] -> BState Index [a]
elemsByNotInds js   = foldrM (\x -> BState . notInds js x) []

indsByElem :: (Eq a) => a -> [a] -> BState Index [Index]
indsByElem y        = foldrM (\x -> BState . indsOf y x) []
indsByElems :: (Eq a) => [a] -> [a] -> BState Index [Index]
indsByElems ys      = foldrM (\x -> BState . indsOfs ys x) []

-- *Main> runReader (liftM2 pp (Reader (\s -> ([1,2], s + 1))) (Reader (\s -> ([3,4], s + 1)))) 2
-- ([1,2,3,4],3)
-- *Main> runReader ((sequence $ [Reader (\s -> ([1,2], s + 1)), Reader (\s -> ([3,4], s + 1))]) >>= \xs -> return $ foldr  (\(xs, s) (zs, _) -> (xs ++ zs, s)) ([], 0) xs) 1
-- ([1,2,3,4],2)
-- Looks promising, doesn't it?
pp :: ([a], Index) -> ([a], Index) -> ([a], Index)
pp (xs, s) (ys, _)  = (xs ++ ys, s)

-- Versions for list of indexes.
-- FIXME: The problem with reusing exisintg notInd is, that notInd outputs
-- resulting accumulator for every index, and there is no way then to
-- disassemble it and determine whether current x was added or not. It should
-- output not resulting accumulator, but monoid. Then i can use `mappend` here
-- to get accumulator. Version with monoid should suit both single index and
-- multiple index versions.
-- Umm.. well, or maybe monad? Maybe monad?
-- Result of notInd should in Maybe monad. Then all result must be binded, so
-- Nothing will zeroing all other values. Then resulting Maybe value should be
-- added to the list: if Nothing, then Nothing added, otherwise unwrap Just
-- and add value. Functions from Data.Maybe, with default value, should do
-- this well.
-- 1. For each j notInd -> [Just x, Nothing, Just x]
-- 2. fold it into Maybe monad.
-- 3. Add result to accumulator using `maybe z (: z)` or fromMaybe.
{-elemByNotInds :: Index -> [a] -> BState Index [a]
elemByNotInds js    = foldrM (\x z -> BState $ notInd i x z) []-}

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

-- Create list of phrase column numbers (indexes) from list of column names.
createOrder :: [String] -> [String] -> [Index]
createOrder refs xs = xs >>= indsByPhrase refs
--createOrder refs    = concat . map (indsByPhrase refs)

reorderPhrases :: [[String]] -> [[String]]
reorderPhrases = undefined

main                =  do
    (file : args) <- getArgs
    handle <- openFile file ReadMode
    contents <- hGetContents handle
    sequence $ map putStrLn $ lines contents
--getContents >>= sequence . map putStrLn . lines

