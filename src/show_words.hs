
import SgfListIndex
import System.IO                -- For hX
import System.Environment       -- For getArgs
import Data.Char                -- For isSpace
--import Data.List                -- For groupBy
--import Data.Maybe               -- For fromJust
--import Data.Monoid
import Control.Applicative      -- For Applicative ((->) a).
--import Control.Monad
--import Control.Monad.State      -- For State monad.
--import Control.Monad.Reader
--import Control.Exception        -- For bracket.
--import qualified Data.ByteString as S
--import qualified Data.ByteString.Lazy as B


-- Reimplement some library functions :-)

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

-- Convert list of lines (list of lists) into list of Line-s. This will
-- reorder elements in lines according to supplied new column order. First
-- line treated as reference (heading) and should contain column names in
-- current order. It will also be reordered.
orderColumns :: (a -> a -> Bool) -> [a] -> [[a]] -> [Line a]
orderColumns _ _ []   = []
orderColumns eq colNames lss@(refs : _) = map (orderList colOrder) lss
  where
    colOrder :: [Index]
    colOrder        = elemsOrder eq refs colNames

-- map for Line datatype.
mapLine :: (a -> b) -> Line a -> Line b
mapLine f (Line xs ys)  = Line (map f xs) (map f ys)

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

joinLineM :: Monad m => (a -> m a) -> (a -> m a) -> Line a -> [m a]
joinLineM f g       = joinLine (>>= f) (>>= g) . mapLine return


-- Phrase separators.
type Phrase         = String
data PhraseSeps     = PhraseSeps
                        { inPhraseSp  :: Char    -- Input separator.
                        , outPhraseSp :: String  -- Output separator.
                        , referenceSp :: String  -- Output heading separaror.
                        }
  deriving (Show)

-- Split each string to phrases by supplied character (omitting separator
-- itself) and remove leading and trailing spaces from each phrase (inner
-- spaces preserved).
splitToPhrases :: Char -> String -> [Phrase]
splitToPhrases sep  = map dropSpaces . splitBy (== sep)
  where
    dropSpaces :: String -> String
    dropSpaces      = dropWhile isSpace . dropWhileEnd isSpace

reorderPhrases :: [String] -> PhraseSeps -> [[Phrase]] -> [Line Phrase]
reorderPhrases colNames (PhraseSeps {referenceSp = rsp}) lss    =
    let (refs' : lss')  = orderColumns (==) colNames lss
        ref'            = concat $ joinLine id (rsp ++) refs'
    in  (orderList [] [ref']) : lss'

putStrF :: String -> IO ()
putStrF x           = putStr x >> hFlush stdout

-- Wait for a key from user.
waitKey :: a -> IO a
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
    putLine :: Line Phrase -> IO ()
    putLine         = sequence_
                        . map (>>= putStrF)
                        . joinLineM f (return . (sp ++))

-- FIXME: Support meainings (subphrases). E.g. it may be seprated by comma and
-- be different translations of a word.
-- FIXME: Use string in inPhraseSps. Maybe implement monadic splitBy, which
-- can track State in supplied eq, and hence split by strings.
-- FIXME: Match partial column names.
-- FIXME: Add "reorder" mode, which is like "print", but do not waits key and
-- just prints all file at once with applied column order.
-- FIXME: Makefile
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
main                = do
    args <- getArgs
    (mode : file : colNames) <- parseArgs args
    contents <- readFile file
    hSetEcho stdin False
    putPhrases (setMode mode) testPhraseSeps
        $ reorderPhrases colNames testPhraseSeps
        $ map (splitToPhrases '-')
        $ lines contents
    putStrLn "Bye!"
  where
    parseArgs :: [String] -> IO [String]
    parseArgs xs@(_ : _ : _)    = return xs
    parseArgs _                 = fail $ "Too few arguments. "
                                    ++ "Usage: ./show_words mode file [column_names]"
    setMode :: String -> (String -> IO String)
    setMode xs
      | xs == "check"   = checkAnswer
      | otherwise       = waitKey

