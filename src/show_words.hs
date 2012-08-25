
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

-- On my system Data.List does not contain dropWhileEnd.
dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd p      = foldr (\x z -> if null z && p x then [] else x : z) []

-- Split list by separator (omitting separator itself).
splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy p []        = []
splitBy p xs        = foldr go [[]] xs
  where
    go x zl@(z : zs)
      | p x         = [] : zl
      | otherwise   = (x : z) : zs

-- Index list.
type Index          = Int
-- Start index.
indBase             = 1
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

-- Folding functions for use in State monads for indexing list.
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

-- Index list by right folding it inside Backward State monad.
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

-- Unwrap monad from list indexing functions.
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

-- Convert list of elements into list of corresponding indexes in "reference"
-- list (preserving order of elements).
elemsOrder :: (a -> a -> Bool) -> [a] -> [a] -> [Index]
elemsOrder eq xs ks = ks >>= indsByElem eq xs


-- Line represents idea of line divided into ordered elements (e.g. by
-- elemsOrder) and other elements (remaining after ordering, i.e. not
-- mentioned in index list for elemsOrder).
-- First list contains ordered elements, second - other elements.
data Line a         = Line [a] [a]
  deriving (Show)

-- Convert list to Line by splitting to list of ordered elements and list of
-- other elements.
orderList :: [Index] -> [a] -> Line a
orderList order     = Line  <$> (\xs -> order >>= elemByInd xs)
                            <*> (\xs -> elemsByNotInds xs order)

-- Convert Line to list and apply some functions to some elements.
-- Function f applied to all ordered elements, except first (e.g. waiting for
-- a key before outputting next element). 
-- Function g is "joining" function. It is applied to all elements (both
-- ordered and others), except first (e.g. prepend spaces to strings to make
-- resulted list suitable for concat).
joinLine :: (a -> a) -> (a -> a) -> Line a -> [a]
joinLine _ _ (Line [] [])       = []
joinLine _ g (Line [] (y : ys)) = y : map g ys
joinLine f g (Line (x : xs) ys) = x : (map (g . f) xs ++ map g ys)

-- map for Line datatype.
mapLine :: (a -> b) -> Line a -> Line b
mapLine f (Line xs ys)  = Line (map f xs) (map f ys)

-- Phrase separators.
type Phrase         = String
data PhraseSeps     = PhraseSeps {
                          inPhraseSp  :: Char    -- Input separator.
                        , outPhraseSp :: String  -- Output separator.
                        , referenceSp :: String  -- Output heading separaror.
                        }
  deriving (Show)

-- Convert list of lines (already split to phrases) into list of Line-s. This
-- will reorder phrases in lines according to supplied new column order. First
-- line treated as reference (heading) - column names in current order. It
-- will also be reordered.
reorderPhrases :: [String] -> PhraseSeps -> [[Phrase]] -> [Line Phrase]
reorderPhrases _ _ []   = []
reorderPhrases colNames (PhraseSeps {referenceSp = rsp}) (refs : lss) =
    let lss'    = map (orderList phrOrder) lss
        refs'   = concat $ joinLine id (rsp ++) $ orderList phrOrder refs
    in  (Line [] [refs']) : lss'
  where
    phrOrder :: [Index]
    phrOrder    = elemsOrder (==) refs colNames

-- Split each string to phrases by supplied character (omitting separator
-- itself) and remove leading and trailing spaces from each phrase (inner
-- spaces preserved).
splitToPhrases :: Char -> String -> [Phrase]
splitToPhrases sep  = map dropSpaces . splitBy (== sep)
  where
    dropSpaces :: String -> String
    dropSpaces      = dropWhile isSpace . dropWhileEnd isSpace

putStrF :: String -> IO ()
putStrF x           = putStr x >> hFlush stdout

-- Wait for a key from user.
waitKey :: String -> IO String
waitKey p           = getChar >> return p

-- Check that user entered correct phrase.
checkAnswer :: Phrase -> IO Phrase
checkAnswer []      = return ""
checkAnswer p       = do
                       r <- getLine
                       return (checkPhrase r ++ p)
  where
    checkPhrase :: String -> String
    checkPhrase r
      | r == p      = " Ura! "
      | otherwise   = " Ops! "

-- Output phrases and execute specified action before every ordered phrase in
-- a line, except first. First is omitted, because it is treated as a
-- question.  Other phrases will be outputted all at once and execution
-- immediately porceeds to next line.
putPhrases :: (String -> IO String) -> PhraseSeps -> [Line Phrase] -> IO ()
putPhrases f (PhraseSeps {outPhraseSp = sp})
                    = mapM_ (\x -> putLine x >> putStrF "\n")
  where
    putLine :: Line Phrase -> IO [()]
    putLine         = sequence
                        . map (>>= putStrF)
                        . joinLine (>>= f) ((sp ++) <$>)
                        . mapLine return

-- FIXME: Support meainings (subphrases). E.g. it may be seprated by comma and
-- be different translations of a word.
-- FIXME: Use string in inPhraseSps. Maybe implement monadic splitBy, which
-- can track State in supplied eq, and hence split by strings.
-- FIXME: Match partial column names.
-- FIXME: Makefile
-- FIXME: Move index list and other library functions to separate file.
-- FIXME: Use foldrM from Data.Foldable ?
-- FIXME: Import only required functions.
-- FIXME: utf8 support.
-- FIXME: Bytestrings.
-- FIXME: Diabled echo for "check" mode is not convenient. Though, if it is
-- enabled, newline will break all output.

testPhraseSeps          = PhraseSeps { inPhraseSp = '-'
                             , outPhraseSp = " : "
                             , referenceSp = " - "
                             }

-- Usage: ./show_words mode file [column_names]
--
--      mode - operation mode:
--          - "print" for waiting for a key after each specified column,
--          - "check" for checking user input against phrase in next specified
--          column (only literal check supported yet).
--      file - file with words.
--      [column_names] - any number of any column names, one in one cmd
--      argument.
--
--   Show words from file one by one in specific order and check your answers
-- against next word.
--
--   File must contain lines of words. Line may contain several columns
-- separated by dash. First line of file treated as heading (reference), and
-- should contain column names in current order. Leading and trailing spaces
-- in each column will be deleted (inner column spaces preserved).
--
--   File will be displayed line by line in the specified columns order. You
-- may specify desired column order at cmd - any column names in any order.
--   If there is at least two valid column names specified, first is treated
-- as "question". Before every other specified column requested action (wait
-- or check) will be executed. All other (non-specified) columns will be
-- outputted at once right after last specified column. No action will be
-- executed before them, and execution immediately proceeds at the next line.
--   Hence, if there is less, than two columns specified, entire file be
-- outputted at once, so this has a little sense :-)
--   Incorrect column names silently skipped without any notification.  This
-- may lead to nasty bugs, when all seems ok, but not works however. So,
-- double check column names in words file and on cmd!
main                =  do
    (mode : file : colNames) <- getArgs
    contents <- readFile file
    hSetEcho stdin False
    putPhrases (setMode mode) testPhraseSeps
        $ reorderPhrases colNames testPhraseSeps
        $ map (splitToPhrases '-')
        $ lines contents
    putStrLn "Bye!"
  where
    setMode :: String -> (String -> IO String)
    setMode xs
      | xs == "check"   = checkAnswer
      | otherwise       = waitKey

