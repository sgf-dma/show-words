
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
indsByElem eq xs e      = indsByElems eq xs [e]


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
inlineSeps (x : xs) = x : (foldl go ([] ++) xs $ [])
  where
    -- Fast implementation of left-associative (++) expression. Inspired by
    -- DiffList newtype from LYaH.
    go :: ([String] -> [String]) -> String -> [String] -> [String]
    go z x          = let x' = outPhrSep ++ x in z . ([x'] ++)

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
reorderPhrases colNames (refs : ls) = let phrOrder = phraseOrder refs colNames
                                      in  map (orderLine phrOrder) ls

putPhrases :: [[String]] -> IO ()
putPhrases lss      = mapM_ putLine1 lss
                    -- = mapM_ (\x -> putPhrase x >> waitKey) $ lss >>= inlineSeps
  where
    -- Append newline into the last string in a line, and prepend spaces into
    -- every other (i add into string itself, but not as new list element).
    inlineSeps :: [String] -> [String]
    inlineSeps (x : [])    = (' ' : x ++ "\n") : []
    inlineSeps (x : xs)    = (' ' : x) : inlineSeps xs
    putPhrase :: String -> IO ()
    putPhrase xs    = putStr xs >> hFlush stdout
    waitKey :: IO ()
    waitKey         = getChar >> return ()
    putLine :: [String] -> IO ()
    putLine (x : [])    = putChar ' ' >> putAndWait x >> putChar '\n'
    putLine (x : xs)    = putChar ' ' >> putAndWait x >> putLine xs
    putAndWait :: String -> IO ()
    putAndWait ps   = putStr ps >> hFlush stdout >> getChar >> return ()
    putLine1 :: [String] -> IO ()
    putLine1 (x : xs)   = putChar ' ' >> putAndGet x >>= putLine1' xs
      where
        putLine1' :: [String] -> String -> IO ()
        putLine1' (x : []) r
          | x == r              = putStr " Ura: " >> putAndGet x >> putChar '\n'
          | otherwise           = putStr " Oops: " >> putAndGet x >> putChar '\n'
        putLine1' (x : xs) r
          | x == r              = putStr " Ura: " >> putAndGet x >>= putLine1' xs
          | otherwise           = putStr " Oops: " >> putAndGet x >>= putLine1' xs
    putAndGet :: String -> IO String
    putAndGet ps    = putStr ps >> hFlush stdout >> getLine


-- FIXME: utf8 support.
-- FIXME: Bytestrings.
main                =  do
    (file : colNames) <- getArgs
    bracket (openFile file ReadMode)
            hClose
            (\h -> do
                contents <- hGetContents h
                hSetEcho stdin False
                putPhrases $ reorderPhrases colNames $ lines contents
                putStrLn "Bye!")

