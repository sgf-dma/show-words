
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

-- Split string by supplied character (omitting separator itself) and remove
-- leading and trailing spaces from each substring (inner spaces preserved).
splitStrBy :: Char -> String -> [String]
splitStrBy sep      = map dropSpaces . splitBy (== sep)
  where
    dropSpaces :: String -> String
    dropSpaces      = dropWhile isSpace . dropWhileEnd isSpace


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

-- map function f over ordered elements and function g over other elements.
mapLine :: (a -> b) -> (a -> b) -> Line a -> Line b
mapLine f g (Line xs ys)   = Line (map f xs) (map g ys)

mapLine1 :: (a -> b) -> Line a -> Line b
mapLine1 f           = mapLine f f

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
joinLineM f g       = joinLine (>>= f) (>>= g) . mapLine1 return


type Column         = String
type Phrase         = String
-- Input separators.
data PhraseSep      = PhraseSep
                        { columnSep  :: Char      -- Column separator.
                        , phraseSep :: Char       -- Phrase separator.
                        , referenceSep :: String  -- Heading separaror.
                        , outPhraseSep :: String  -- Output separator.
                        , outMeaningSep :: String -- Output meaning separator.
                        }
  deriving (Show)

-- Split input lines into columns, order columns (convert to Line-s) according
-- to supplied new column order and split columns to phrases.
reorderColumns :: [Column] -> PhraseSep -> [String] -> [Line [Phrase]]
reorderColumns colNames (PhraseSep { columnSep = sp
                                   , phraseSep = msp
                                   , referenceSep = rsp})
                    = splitToPhrases
                        . makeRef
                        . orderColumns (==) colNames -- :: [Line Column]
                        . splitToColumns             -- :: [[Column]]
  where
    splitToColumns :: [String] -> [[Column]]
    splitToColumns  = map (splitStrBy sp)
    makeRef :: [Line Column] -> [Line Column]
    makeRef []          = []
    makeRef (refl : xs) = let ref' = concat $ joinLine id (rsp ++) refl
                          in  (orderList [] [ref']) : xs
    splitToPhrases :: [Line Column] -> [Line [Phrase]]
    splitToPhrases  []  = []
    splitToPhrases (ref : xs)
                    = mapLine1 (: []) ref : map (mapLine1 (splitStrBy msp)) xs


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

-- Output phrases and execute specified action before every phrase in ordered
-- column, except first ordered column. First is omitted, because it is
-- treated as a question.  Other columns (and phrases they contain) will be
-- outputted all at once and execution immediately porceeds to next line.
putPhrases :: (Phrase -> IO Phrase) -> PhraseSep -> [Line [Phrase]] -> IO ()
putPhrases f (PhraseSep {outPhraseSep = sp, phraseSep = msp})
                    = mapM_ (\x -> putLine x >> putStrF "\n")
  where
    putLine :: Line [Phrase] -> IO ()
    putLine = sequence_
                . map (>>= putStrF)
                . concat
                . map joinPhrases
                . joinLine (map (>>= f)) joinColumns -- :: -> [[IO Phrase]]
                . mapLine1 (map return)              -- :: -> Line [IO Phrase]
      where
        joinColumns :: [IO Phrase] -> [IO Phrase]
        joinColumns []          = [return sp]
        joinColumns (mx : mxs)  = ((sp ++) <$> mx) : mxs
        joinPhrases :: [IO Phrase] -> [IO Phrase]
        joinPhrases []          = []
        joinPhrases (mx : mxs)  = mx : map ((", " ++) <$>) mxs

-- FIXME: Use the same separators for input and output. Really, there is no
-- sense in changing separatos - the one user wants should be in the file
-- already. But this also mean, that reference in input will be separated by
-- different char, And this also means, that separatos must already contain
-- spaces, so it must be string.. ugh.
-- FIXME: Use string in inPhraseSps. Maybe implement monadic splitBy, which
-- can track State in supplied eq, and hence split by strings.
-- FIXME: Match partial column names.
-- FIXME: Left main for user config and move all other code in functions and
-- modules, like SgfPhrases, etc.
-- FIXME: Makefile
-- FIXME: Import only required functions.
-- FIXME: utf8 support.
-- FIXME: Bytestrings.
-- FIXME: Diabled echo for "check" mode is not convenient. Though, if it is
-- enabled, newline will break all output.

testPhraseSep       = PhraseSep { columnSep = '-'
                                , phraseSep = ','
                                , outPhraseSep = " : "
                                , outMeaningSep = ", "
                                , referenceSep = " - "
                                }

-- Usage: ./show_words mode file [column_names]
--
--      mode - operation mode:
--          - "print" for waiting for a key after each specified column,
--          - "check" for checking user input against each phrase in next
--          specified columns (only literal check supported yet).
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
    putPhrases (setMode mode) testPhraseSep
        $ reorderColumns colNames testPhraseSep
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
      | xs == "print"   = waitKey
      | otherwise       = return

{-
-- Outdated code.

reorderPhrases :: [Phrase] -> PhraseSep -> [[Phrase]] -> [Line Phrase]
reorderPhrases colNames (PhraseSep {referenceSep = rsp}) xss    =
    let (refl' : xls')  = orderColumns (==) colNames xss
        ref'            = concat $ joinLine id (rsp ++) refl'
    in  (orderList [] [ref']) : xls'

putPhrases :: (String -> IO String) -> PhraseSep -> [Line Phrase] -> IO ()
putPhrases f (PhraseSep {outPhraseSep = sp})
                    = mapM_ (\x -> putLine x >> putStrF "\n")
  where
    putLine :: Line Phrase -> IO ()
    putLine         = sequence_
                        . map (>>= putStrF)
                        . joinLineM f (return . (sp ++))

main                = do
    args <- getArgs
    (mode : file : colNames) <- parseArgs args
    contents <- readFile file
    hSetEcho stdin False
    putPhrases (setMode mode) testPhraseSep
        $ reorderPhrases colNames testPhraseSep
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
      | xs == "print"   = waitKey
      | otherwise       = return -}

