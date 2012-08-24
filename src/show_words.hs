
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
import Control.Exception        -- For bracket.
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as B

-- Reimplement some library functions :-)

-- Monadic folds.
foldrM :: (Monad m) => (a -> b -> m b) -> b -> [a] -> m b
foldrM g z []       =  return z
foldrM g z (x : xs) =  foldrM g z xs >>= g x
foldlM              :: (Monad m) => (b -> a -> m b) -> b -> [a] -> m b
foldlM g z []       =  return z
foldlM g z (x : xs) =  g z x >>= \z' -> foldlM g z' xs

-- On my system Data.List does not contain dropWhileEnd.
dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd p      = foldr (\x z -> if null z && p x then [] else x : z) []

-- Start index (for State monads).
indBase             = 1

-- Split list. {{{

-- Reverse of splitAt (count elements from list end).
splitAtEndM :: Index -> [a] -> State Index ([a], [a])
splitAtEndM i xs    = foldrM go ([], []) xs
  where
    go :: a -> ([a], [a]) -> State Index ([a], [a])
    go x (z1, z2)       = State $ \s -> let z' = if s <= i then (z1, x : z2)
                                                   else         (x : z1, z2)
                                        in  (z', s + 1)

-- Unwrap State monad.
splitAtEnd :: Index -> [a] -> ([a], [a])
splitAtEnd i xs         = fst $ runState (splitAtEndM i xs) indBase

-- Split list by separator (omitting separator itself).
splitBy :: (Eq a) => a -> [a] -> [[a]]
splitBy s []        = []
splitBy s xs        = foldr go [[]] xs
  where
    go x zl@(z : zs)
      | x == s       = [] : zl
      | otherwise    = (x : z) : zs

-- END Split list. }}}
-- Index list. {{{

type Index          =  Int
-- Backward state monad from "The essence of functional programming" by Philip
-- Wadler.
newtype BState s a  =  BState {runBState :: (s -> (a, s))}
instance Monad (BState s) where
    return x        =  BState (\s -> (x, s))
    BState m >>= f  =  BState $ \s2 ->
                        let (x, s0)   = m s1
                            BState m' = f x
                            (x', s1)  = m' s2
                        in  (x', s0)

-- Folding functions for use in State monads for indexing list. {{{
-- 
-- Add list element x to the accumulator z only if its index s equals to i.
onlyInds :: [Index] -> a -> [a] -> Index -> ([a], Index)
onlyInds js x z     = \s -> let z' = if s `elem` js then x : z else z
                            in  (z', s + 1)

-- Complement to onlyInd. Add each list elements x to accumulator z if its
-- index s not equals to i.
notInds :: [Index] -> a -> [a] -> Index -> ([a], Index)
notInds js x z      = \s -> let z' = if s `notElem` js then x : z else z
                            in  (z', s + 1)

indsOfs :: (a -> [a] -> Bool) -> [a] -> a -> [Index] -> Index -> ([Index], Index)
indsOfs elem' ks x z  = \s -> let z' = if x `elem'` ks then s : z else z
                              in  (z', s + 1)

-- END Folding functions for use in State monads for indexing list. }}}
-- Index list by right folding it inside Backward State monad. {{{

-- Choose all elements from list which indexes are in the specified index
-- list.
elemsByIndsM :: [Index] -> [a] -> BState Index [a]
elemsByIndsM js     = foldrM (\x -> BState . onlyInds js x) []

-- Complement to elemsByInds. Choose all elements from list which indexes are
-- not in the sepcified index list.
elemsByNotIndsM :: [Index] -> [a] -> BState Index [a]
elemsByNotIndsM js  = foldrM (\x -> BState . notInds js x) []

-- Reverse of elemsByInds. Choose all indexes, which elements from the
-- specified list have.
indsByElemsM :: (a -> a -> Bool) -> [a] -> [a] -> BState Index [Index]
indsByElemsM eq ks  = foldrM (\x -> BState . indsOfs elem' ks x) []
  where elem' y     = foldr (\x z -> if x `eq` y then True else z) False

-- END Index list by folding inside Backward State monad. }}}
-- Unwrap monad from list indexing functions. {{{

elemsByInds :: [a] -> [Index] -> [a]
elemsByInds xs js       = fst $ runBState (elemsByIndsM js xs) indBase

elemsByNotInds :: [a] -> [Index] -> [a]
elemsByNotInds xs js    = fst $ runBState (elemsByNotIndsM js xs) indBase

indsByElems :: (a -> a -> Bool) -> [a] -> [a] -> [Index]
indsByElems eq xs ks    = fst $ runBState (indsByElemsM eq ks xs) indBase

elemByInd :: [a] -> Index -> [a]
elemByInd xs j          = elemsByInds xs [j]

indsByElem :: (a -> a -> Bool) -> [a] -> a -> [Index]
indsByElem eq xs k      = indsByElems eq xs [k]

-- END Unwrap monad from list index functions. }}}

-- END Index list. }}}

-- Phrase separators.
phrSep              = '-'
outPhrSep           = " : "

-- Split each string to phrases by phrSep character (omitting separator
-- itself) and remove leading and trailing spaces from each phrase (phrase may
-- contain spaces).
splitToPhrases :: String -> [String]
splitToPhrases      = map dropSpaces . splitBy phrSep
  where
    dropSpaces :: String -> String
    dropSpaces      = dropWhile isSpace . dropWhileEnd isSpace

-- Convert list of column names to list of column numbers (preserving order).
-- Use reference string separated by phrSep for recognizing column names.
phraseOrder :: String -> [String] -> [Index]
phraseOrder refs colNames   = colNames >>= indsByElem (==) refs'
  where refs'               = splitToPhrases refs

-- Inline separators into phrases (add into list element itself, hence no new
-- elements is added to the list). Prepend outPhrSep to all phrases, except
-- first (in the list). 
inlineSeps :: [String] -> [String]
inlineSeps []        = []
inlineSeps (xa : xs) = xa : (foldl go ([] ++) xs $ [])
  where
    -- Fast implementation of left-associative (++) expression. Inspired by
    -- DiffList newtype from LYaH.
    go :: ([String] -> [String]) -> String -> [String] -> [String]
    go z x          = let x' = outPhrSep ++ x in z . ([x'] ++)

-- Do not inline separators in empty strings.
-- FIXME: Can i drop first version (inlineSeps) and use this everywhere?
inlineSepsNE :: [String] -> [String]
inlineSepsNE []         = []
inlineSepsNE (xa : xs)  = xa : (foldl go ([] ++) xs $ [])
  where
    -- Fast implementation of left-associative (++) expression. Inspired by
    -- DiffList newtype from LYaH.
    go :: ([String] -> [String]) -> String -> [String] -> [String]
    go z []         = z . ([""] ++)
    go z x          = let x' = outPhrSep ++ x in z . ([x'] ++)

-- Convert line to list of phrases in specified order. All phrases not
-- specified in phrase order will be joined (using inlineSeps) into one last
-- phrase.
orderLine :: [Index] -> String -> [String]
orderLine phrOrder  = ((++) <$> orderedPhrases <*> otherPhrases) . splitToPhrases
  where
    orderedPhrases :: [String] -> [String]
    orderedPhrases ps   = phrOrder >>= elemByInd ps
    -- Even if there is no other phrases, otherPhrases will be empty String,
    -- but not empty list. And, hence, list returned by orderLine will always
    -- contains otherPhrases as last (may be empty and may be only) element.
    otherPhrases :: [String] -> [String]
    otherPhrases ps = (concat $ inlineSeps $ elemsByNotInds ps $ phrOrder) : []

-- Reorder phrases in each line according to column names list.  First line
-- must contain column names in current order (it will not be printed). Not
-- specified columns will be joined into one last phrase at each line.
reorderPhrases :: [String] -> [String] -> [[String]]
reorderPhrases _ []                 = [[]]
reorderPhrases colNames (refs : ls) = let phrOrder = phraseOrder refs colNames
                                          --refs' = elemsByInds refs phrOrder
                                      in  map (orderLine phrOrder) ls

putStrF :: String -> IO ()
putStrF x           = putStr x >> hFlush stdout

-- Wait for a key from user.
waitKey :: String -> IO ()
waitKey _           = getChar >> return ()

-- Check that user entered correct phrase.
checkAnswer :: String -> IO ()
checkAnswer p       = do
                        r <- getLine
                        putStr $ checkPhrase r
  where
    checkPhrase :: String -> String
    checkPhrase r
      | r' == p     = " Ura: "
      | otherwise   = " Ops: "
      where
        -- Add separator to user response. Otherwise, i can't match literally.
        r' :: String
        r'          = last $ inlineSeps ("" : [r])

-- Output phrases and execute specified action before every phrase in a line,
-- except first. First is omitted, because it may be treated as question.
putPhrases :: (String -> IO ()) -> [[String]] -> IO ()
putPhrases f            = mapM_ (putLine . inlineSepsNE)
  where
    putLine :: [String] -> IO ()
    putLine []          = return ()
    putLine [xa]
      | null xa         = return ()
      | otherwise       = putStrF (xa ++ "\n")
    putLine (xa : xs)   = do
                            putStrF xa
                            let (xs', [xb]) = splitAtEnd indBase xs
                            mapM_ (\x -> f x >> putStrF x) xs'
                            putStrF (xb ++ "\n")

-- FIXME: Print heading line, which shows current column order.
-- FIXME: Makefile
-- FIXME: Split index list and other library functions to separate file.
-- FIXME: Match partial column names.
-- FIXME: Use foldrM from Data.Foldable ?
-- FIXME: Import only required functions.
-- FIXME: utf8 support.
-- FIXME: Bytestrings.
-- FIXME: readFile instead of bracket.
-- FIXME: Diabled echo for "check" mode is not convenient. Though, if it is
-- enabled, newline will break all output.

-- Usage: ./show_words mode file [column_names]
--
-- Be aware, that incorrect column names silently skipped without any
-- notification.  This may lead to nasty bugs, when all seems ok, but not
-- works however. So, double check column names in words file and on cmd!
main                =  do
    (mode : file : colNames) <- getArgs
    bracket (openFile file ReadMode)
            hClose
            (\h -> do
                contents <- hGetContents h
                hSetEcho stdin False
                putPhrases (setMode mode) $ reorderPhrases colNames $ lines contents
                putStrLn "Bye!")
  where
    setMode :: String -> (String -> IO ())
    setMode xs
      | xs == "check"   = checkAnswer
      | otherwise       = waitKey

