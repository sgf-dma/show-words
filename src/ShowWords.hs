
module ShowWords
    ( WordsSeps(..)
    , showWords)
  where

import System.IO                -- For hSetEcho, hFlush, stdin, stdout.
import Codec.Binary.UTF8.String -- For encode, decode.
import qualified Data.ByteString.Lazy as B
import System.Environment       -- For getArgs.
import System.Console.GetOpt    -- For getOpt.
import Data.Char                -- For isSpace.
import Control.Applicative      -- For Applicative ((->) a), <$> and other.
import System.Random            -- For randomRs.

import SgfList
import SgfOrderedLine
import ShowWordsText

putStrF :: String -> IO ()
putStrF x           = do
                        B.putStr $ B.pack $ encode x
                        hFlush stdout

-- Wait for a key from user.
waitKey :: a -> IO a
waitKey p           = getChar >> return p

-- Check that user entered correct phrase.
checkAnswer :: Phrase -> IO Phrase
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
putPhrases :: (Phrase -> IO Phrase) -> WordsSeps -> [Line [Phrase]] -> IO ()
putPhrases f (WordsSeps {getColumnSep = colSp, getPhraseSep = phrSp})
                    = mapM_ (\x -> putLine x >> putStrF "\n")
  where
    putLine :: Line [Phrase] -> IO ()
    putLine = mapM_ (>>= putStrF)
                . concatMap joinPhrases               -- :: -> [IO Phrase]
                . joinLine (map (>>= f')) joinColumns -- :: -> [[IO Phrase]]
                . mapLine1 (map return)               -- :: -> Line [IO Phrase]
      where
        f' :: Phrase -> IO Phrase
        f' []   = return []
        f' xs   = f xs
        joinColumns :: [IO Phrase] -> [IO Phrase]
        joinColumns []          = [return colSp]
        joinColumns (mx : mxs)  = ((colSp ++) <$> mx) : mxs
        joinPhrases :: [IO Phrase] -> [IO Phrase]
        joinPhrases []          = []
        joinPhrases (mx : mxs)  = mx : map ((phrSp ++) <$>) mxs


-- Usage: ./show_words [options..] [column_names]
--
--   Show words (phrases) from file one by one in specified column order and
-- check your answers against next word.
--
-- Options:
--  -m, --mode MODE - set operation mode to MODE.
--  -f, --file FILE - read words from file.
--  -s              - shuffle input lines.
--  where
--      MODE - operation mode:
--          - "print" for waiting for a key after each next phrase in
--            specified columns.
--          - "check" for checking user input against each next phrase in
--            specified columns (literal check).
--          - "reorder" just reorder columns and output all file at once. This
--            is default.
--      FILE - input file with words (words.txt is by default).
--      [column_names] - any number of any column names, one in one command
--                       line argument.
--
--   File must contain lines of words. Line may contain several columns
-- separated by columnSep. Column may contain several phrases separated by
-- phraseSep.  First line of file treated as heading (reference), and should
-- contain column names in current order separated by referenceSep. These
-- separators defined in main.
--
--   File will be displayed line by line, phrase by phrase in the specified
-- columns order with leading and trailing spaces of each phrase deleted
-- (inner spaces preserved). You may specify desired column order at command
-- line: any column names in any order.
--
--   If there is at least two valid column names specified, first is treated
-- as "question". Before every phrase in other (not first) specified columns
-- requested action (wait or check) will be executed. All other
-- (non-specified) columns will be outputted at once right after last
-- specified column. No action will be executed before them or phrases in
-- them, and execution immediately proceeds at the next line.
--   Hence, if there is less, than two columns specified, entire file will be
-- outputted at once, so this has a little sense :-)
--
--   Incorrect column names silently skipped without any notification.  This
-- may lead to nasty bugs, when all seems ok, but not works however. So,
-- double check column names in words file and on command line!

-- Set appropriate operation mode (how to put phrases) from string.
setMode :: String -> Phrase -> IO Phrase
setMode xs
  | xs == "check"   = checkAnswer
  | xs == "print"   = waitKey
--  | xs == "reorder" = return
  | otherwise       = return

data Options        = Options
                        { optMode      :: Phrase -> IO Phrase
                        , optLineOrder :: String
                        , optFile      :: FilePath
                        }

defaultOpts :: Options
defaultOpts         = Options
                        { optMode       = setMode "default"
                        , optLineOrder  = "default"
                        , optFile       = "./words.txt"
                        }

optsDescr :: [OptDescr (Options -> Options)]
optsDescr = 
    [ Option    ['m']
                ["mode"]
                (ReqArg (\xs opt -> opt {optMode = setMode xs}) "MODE")
                ("Set operation mode to MODE ('reorder' by default).\n"
                    ++ "Valid values are 'print', 'check', 'reorder'.")
    , Option    ['s']
                ["shuffle"]
                (NoArg (\opt -> opt {optLineOrder = "shuffle"}))
                "Shuffle lines."
    , Option    ['f']
                ["file"]
                (ReqArg (\xs opt -> opt {optFile = xs}) "FILE")
                "Read words from FILE (words.txt by default)."
    ]

-- Parse command-line arguments.
parseArgs :: [String] -> IO (Options, [String])
parseArgs argv      = case getOpt Permute optsDescr argv of
                        (xs, ys, []) ->
                            return (foldl (\z f -> f z) defaultOpts xs, ys)
                        (_, _, errs) ->
                            fail (concat errs ++ usageInfo header optsDescr)
  where header      = "Usage: show_words [OPTION...] columnNames.."

-- FIXME: Change sequence:
--  splitToColumns -> ToPhrases -> reorderColumns.
-- After all, if reordercolumns written correctly, this should have no
-- difference when i call it. But such sequence allows to use new
-- splitToColumns instead of splitToPhrases.
-- Show words (main function).
showWords :: WordsSeps -> IO ()
showWords wSps = do
    argv <- getArgs
    (Options
          { optMode = mode
          , optLineOrder = lineOrder
          , optFile = file}
      , colNames) <- parseArgs argv
    contents <- readFile' file
    hSetEcho stdin False
    -- FIXME: Applicative (->) with wSps ?
    xs <- reorderLines lineOrder
            $ map (mapLine1 (map dropSpaces))
            $ splitToPhrases wSps
            $ reorderColumns colNames wSps
            $ splitToColumns wSps
            $ lines contents
    putPhrases mode wSps xs
    putStrF "Bye!\n"
  where
    readFile' :: FilePath -> IO String
    readFile' file  = do
        contents <- B.readFile file
        return $ decode $ B.unpack contents

