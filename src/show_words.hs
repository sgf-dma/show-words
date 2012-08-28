
import SgfListIndex
import System.IO                -- For hX
import System.Environment       -- For getArgs
import Data.Char                -- For isSpace
import Data.List                -- For groupBy
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

-- FIXME: Use type Column instead of Phrase and Phrase instead of Meaning.
type Phrase         = String
type Meaning        = String
-- Phrase separators.
data PhraseSep      = PhraseSep
                        { phraseSep  :: Char      -- Input phrase separator.
                        , meaningSep :: Char      -- Input meaning separator.
                        , outPhraseSep :: String  -- Output separator.
                        , outMeaningSep :: String -- Output meaning separator.
                        , referenceSep :: String  -- Output heading separaror.
                        }
  deriving (Show)

-- FIXME: Rename to splitBy1.
-- Split each string to phrases by supplied character (omitting separator
-- itself) and remove leading and trailing spaces from each phrase (inner
-- spaces preserved).
splitToPhrases :: Char -> String -> [Phrase]
splitToPhrases sep  = map dropSpaces . splitBy (== sep)
  where
    dropSpaces :: String -> String
    dropSpaces      = dropWhile isSpace . dropWhileEnd isSpace

reorderPhrases :: [Phrase] -> PhraseSep -> [[Phrase]] -> [Line Phrase]
reorderPhrases colNames (PhraseSep {referenceSep = rsp}) xss    =
    let (refl' : xls')  = orderColumns (==) colNames xss
        ref'            = concat $ joinLine id (rsp ++) refl'
    in  (orderList [] [ref']) : xls'

reorderPhrases1 :: [Phrase] -> PhraseSep -> [Line [Phrase]] -> [Line (Line Phrase)]
reorderPhrases1 _ _ xls    = map (mapLine1 (orderList [])) xls

reorderPhrases2 :: [Phrase] -> PhraseSep -> [String] -> [Line (Line Phrase)]
reorderPhrases2 colNames (PhraseSep {referenceSep = rsp, phraseSep = sp}) xs =
    let (refl' : xls')  = orderPhrases xs
        ref'            = concat $ joinLine id (rsp ++) refl'
        refl''          = mapLine1 (orderList []) $ orderList [] [[ref']]
    in  refl'' : orderMeanings xls'
  where
    orderPhrases :: [String] -> [Line Phrase]
    orderPhrases    = orderColumns (==) colNames . map (splitToPhrases sp)
    orderMeanings :: [Line Phrase] -> [Line (Line Phrase)]
    orderMeanings   = map (mapLine1 (orderList [] . splitToPhrases sp))

{-
map (mapLine1 (orderList []))    :: [Line (Line Phrase)]
. (\(ref : ) -> [ref] : splitToPhrases) :: [Line [Phrase]]
. makeRef                       :: [Line Phrase]
. orderColumns (==) colNames    :: [Line Phrase]
. map (splitToPhrases sp)       :: [[Phrase]]-}

reorderPhrases3 :: [Phrase] -> PhraseSep -> [String] -> [Line (Line Meaning)]
reorderPhrases3 colNames (PhraseSep { phraseSep = sp
                                    , meaningSep = msp
                                    , referenceSep = rsp}) =
    --map (mapLine (\xs -> orderList (listIndices xs) xs) (orderList []))
    map (mapLine (flip orderList <*> listIndices) (orderList [])) -- :: [Line (Line Phrase)]
    . (\(ref : xs) -> mapLine1 (: []) ref : map splitToMeanings xs) -- :: [Line [Phrase]]
    . makeRef                       -- :: [Line Phrase]
    . orderColumns (==) colNames    -- :: [Line Phrase]
    . map (splitToPhrases sp)       -- :: [[Phrase]]
  where
    makeRef :: [Line Phrase] -> [Line Phrase]
    makeRef (refl : xs) = let ref' = concat $ joinLine id (rsp ++) refl
                          in  (orderList [] [ref']) : xs
    splitToMeanings :: Line Phrase -> Line [Meaning]
    splitToMeanings = mapLine1 (splitToPhrases msp)

reorderPhrases4 :: [Phrase] -> PhraseSep -> [String] -> [Line [Phrase]]
reorderPhrases4 colNames (PhraseSep { phraseSep = sp
                                    , meaningSep = msp
                                    , referenceSep = rsp}) =
    (\(ref : xs) -> mapLine1 (: []) ref : map splitToMeanings xs) -- :: [Line [Phrase]]
    . makeRef                       -- :: [Line Phrase]
    . orderColumns (==) colNames    -- :: [Line Phrase]
    . map (splitToPhrases sp)       -- :: [[Phrase]]
  where
    makeRef :: [Line Phrase] -> [Line Phrase]
    makeRef (refl : xs) = let ref' = concat $ joinLine id (rsp ++) refl
                          in  (orderList [] [ref']) : xs
    splitToMeanings :: Line Phrase -> Line [Meaning]
    splitToMeanings = mapLine1 (splitToPhrases msp)

{-
reorderPhrases1 :: [Phrase] -> PhraseSep -> [String] -> [Line Phrase]
reorderPhrases1 colNames (PhraseSep {referenceSep = rsp}) lss    =
    let (refs' : lss')  = orderColumns (==) colNames $ map splitToPhrases) lss
        ref'            = concat $ joinLine id (rsp ++) refs'
    in  (orderList [] [ref']) : lss'-}



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
putPhrases :: (String -> IO String) -> PhraseSep -> [Line Phrase] -> IO ()
putPhrases f (PhraseSep {outPhraseSep = sp})
                    = mapM_ (\x -> putLine x >> putStrF "\n")
  where
    putLine :: Line Phrase -> IO ()
    putLine         = sequence_
                        . map (>>= putStrF)
                        . joinLineM f (return . (sp ++))

putPhrases4 :: (String -> IO String) -> PhraseSep -> [Line [Phrase]] -> IO ()
putPhrases4 f (PhraseSep {outPhraseSep = sp, meaningSep = msp})
                    = mapM_ (\x -> putLine x >> putStrF "\n")
  where
    putLine :: Line [Phrase] -> IO ()
    putLine = sequence_
                . map (>>= putStrF)
                . concat
                . map joinPhrases
                . joinLine (map (>>= f)) joinColumns -- :: -> [[IO Phrase]]
                           --(\(mx : mxs) -> return sp : mx : map ((", " ++) <$>) mxs)
                           --(\(mx : mxs) -> ((sp ++) <$> mx) : map ((", " ++) <$>) mxs)
                . mapLine1 (map return)
      where
        joinColumns :: [IO Phrase] -> [IO Phrase]
        joinColumns []          = [return sp]
        joinColumns (mx : mxs)  = ((sp ++) <$> mx) : mxs
        joinPhrases :: [IO Phrase] -> [IO Phrase]
        joinPhrases []          = []
        joinPhrases (mx : mxs)  = mx : map ((", " ++) <$>) mxs

{-
putPhrases3 :: (String -> IO String) -> PhraseSep -> [Line (Line Meaning)] -> IO ()
putPhrases3 f (PhraseSep {outPhraseSep = sp, outMeaningSep = msp})
                    = mapM_ (\x -> putLine x >> putStrF "\n")
  where
    putLine :: Line (Line Meaning) -> IO ()
    putLine         = sequence_
                        . map (>>= putStrF)
                        . joinLineM (>>= mapLine1 f) (return . (sp ++))-}

-- FIXME: Use the same separators for input and output. Really, there is no
-- sense in changing separatos - the one user wants should be in the file
-- already. But this also mean, that reference in input will be separated by
-- different char, And this also means, that separatos must already contain
-- spaces, so it must be string.. ugh.
-- FIXME: Support meainings (subphrases). E.g. it may be seprated by comma and
-- be different translations of a word.
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

testPhraseSep       = PhraseSep { phraseSep = '-'
                                , meaningSep = ','
                                , outPhraseSep = " : "
                                , outMeaningSep = ", "
                                , referenceSep = " - "
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
    putPhrases4 (setMode mode) testPhraseSep
        $ reorderPhrases4 colNames testPhraseSep
        $ lines contents
{-
    putPhrases (setMode mode) testPhraseSep
        $ reorderPhrases colNames testPhraseSep
        $ map (splitToPhrases '-')
        $ lines contents-}
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

