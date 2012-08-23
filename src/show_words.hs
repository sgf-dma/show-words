
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

-- Indexed list.
type Index          =  Int

-- Folding functions for use in State or Backward State monads.
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

-- Index list by folding inside Backward State monad.
--
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


-- Start index.
indBase             = 1

-- Unwrap monad from list index functions.
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

-- Inline separators into phrases. Prepend outPhrSep to all phrases, except
-- first (in the list). No new elements is added to the list.
inlineSeps :: [String] -> [String]
inlineSeps []        = []
inlineSeps (x0 : xs) = x0 : (foldl go ([] ++) xs $ [])
  where
    -- Fast implementation of left-associative (++) expression. Inspired by
    -- DiffList newtype from LYaH.
    go :: ([String] -> [String]) -> String -> [String] -> [String]
    go z x          = let x' = outPhrSep ++ x in z . ([x'] ++)

-- Omit empty strings from resulting list.
inlineSepsNE :: [String] -> [String]
inlineSepsNE        = inlineSeps . filter (not . null)

-- Convert line to list of phrases in specified order. All phrases not
-- specified in phrase order will be joined (using inlineSeps) into one last
-- phrase.
orderLine :: [Index] -> String -> [String]
orderLine phrOrder  = ((++) <$> orderedPhrases <*> otherPhrases) . splitToPhrases
  where
    orderedPhrases :: [String] -> [String]
    orderedPhrases ps   = phrOrder >>= elemByInd ps
    otherPhrases :: [String] -> [String]
    otherPhrases ps = (concat $ inlineSeps $ elemsByNotInds ps $ phrOrder) : []

-- Reorder phrases in each line according to column names list.  First line
-- must contain column names in current order (it will not be printed). Not
-- specified columns will be joined into one last phrase at each line.
reorderPhrases :: [String] -> [String] -> [[String]]
reorderPhrases _ []                 = [[]]
reorderPhrases colNames (refs : ls) = let phrOrder = phraseOrder refs colNames
                                      in  map (orderLine phrOrder) ls

putStrF :: String -> IO ()
putStrF x           = putStr x >> hFlush stdout

-- Wait for a user pressing key.
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
      | r == p      = "Ura: "
      | otherwise   = "Ops: "

-- FIXME: Last line, when it was not mentioned in column spec, should also be
-- just outputted.
-- Output phrases and execute specified action before every phrase in a line,
-- except first. First is omitted, because it may be treated as question.
putPhrases :: (String -> IO ()) -> [[String]] -> IO ()
putPhrases f        = mapM_ (putLine . inlineSepsNE)
  where
    putLine :: [String] -> IO ()
    putLine []       = return ()
    putLine (x0 : xs) = do
                        putStrF x0
                        mapM_ (\x -> f x >> putStrF x) xs
                        putStrF "\n"

-- FIXME: utf8 support.
-- FIXME: Bytestrings.
-- FIXME: Diabled echo for "check" mode is not convenient. Though, if it is
-- enabled, newline will break all output.. This is the problem, really.
-- Usage: ./show_words mode file [column_names]
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

