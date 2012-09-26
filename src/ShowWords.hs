
module ShowWords
    ( WordsSeps (..)
    , showWords)
  where

import System.IO                -- For hSetEcho, hFlush, stdin, stdout.
import Codec.Binary.UTF8.String -- For encode, decode.
import qualified Data.ByteString.Lazy as B
import System.Environment       -- For getArgs.
import System.Console.GetOpt    -- For getOpt.
import Control.Applicative      -- For Applicative ((->) a), <$> and other.

import SgfOrderedLine
import ShowWordsText
import ShowWordsOutput

-- FIXME: May be revert type Column? But this time in the form of [String]?
-- FIXME: Add statistic? WriterT w IO, is it not?
-- FIXME: Mark place, where column match (literal or not) performed.
-- Also, mark place, where separator matches preformed, as well as answer
-- matches.
-- FIXME: Several output formats. Increase number of lines (on which one input
-- line split):
--      - line by line (current);
--      - column by line;
--      - phrase by line;
-- FIXME: Empty answer == skip answer, but do not check.
-- FIXME: Tests.
-- FIXME: I disabled prefix match for column names during testing.
-- FIXME: Use '-' for read words from stdin. But stdin is used for interaction
-- with user..
-- FIXME: Not literal match for separators?
-- FIXME: Disabled echo for "check" mode is not convenient. Though, if it is
-- enabled, newline will break all output.


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
setMode :: String -> String -> IO String
setMode xs
  | xs == "check"   = checkAnswer
  | xs == "print"   = waitKey
--  | xs == "reorder" = return
  | otherwise       = return

data Options        = Options
                        { optMode      :: String -> IO String
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
            $ map (fmap (map dropSpaces))
            $ splitToPhrases wSps
            $ reorderColumns refEq colNames
            $ splitToColumns wSps
            $ lines contents
    putPhrases mode wSps xs
    putStrF "Bye!\n"
  where
    -- Equality test for reference columns.
    refEq :: String -> String -> Bool
    refEq xs ys     = dropSpaces xs == dropSpaces ys
    readFile' :: FilePath -> IO String
    readFile' file  = do
        contents <- B.readFile file
        return $ decode $ B.unpack contents

