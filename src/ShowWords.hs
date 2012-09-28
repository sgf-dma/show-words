
module ShowWords
    ( WordsSeps (..)
    , showWords
    , showWords1
    , showWords0
    )
  where

import System.IO                -- For hSetEcho, hFlush, stdin, stdout.
import Codec.Binary.UTF8.String -- For encode, decode.
import qualified Data.ByteString.Lazy as B
import System.Environment       -- For getArgs.
import System.Console.GetOpt    -- For getOpt.
import Control.Monad.Reader

import ShowWordsOptions
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

defaultOpts :: Options
defaultOpts         = Options
                        { optMode         = setMode "default"
                        , optLineOrder    = "default"
                        , optFile         = "./words.txt"
                        , optColumnSep    = " : "
                        , optPhraseSep    = "; "
                        , optReferenceSep = " - "
                        }

optsDescr :: [OptDescr (Options -> Options)]
optsDescr = 
    [ Option    ['m']
                ["mode"]
                (ReqArg (\mode opt -> opt {optMode = setMode mode}) "MODE")
                ("Set operation mode to MODE (default 'reorder').\n"
                    ++ "Valid values are 'print', 'check', 'reorder'."
                )
    , Option    ['s']
                ["shuffle"]
                (NoArg (\opt -> opt {optLineOrder = "shuffle"}))
                "Shuffle lines (default disabled)."
    , Option    ['f']
                ["file"]
                (ReqArg (\file opt -> opt {optFile = file}) "FILE")
                "Read words from FILE (default 'words.txt')."
    , Option    ['c']
                ["column-sep"]
                (ReqArg (\colSp opt -> opt {optColumnSep = colSp})
                        "COLUMN_SEP"
                )
                "Column separator (default \" : \")."
    , Option    ['p']
                ["phrase-sep"]
                (ReqArg (\phrSp opt -> opt {optPhraseSep = phrSp})
                        "PHRASE_SEP"
                )
                "Phrase separator (default \"; \")."
    , Option    ['r']
                ["reference-sep"]
                (ReqArg (\refSp opt -> opt {optReferenceSep = refSp})
                        "REFERENCE_SEP"
                )
                "Reference (heading) separator (default \" - \")."
    ]

-- Parse command-line arguments.
parseArgs :: [String] -> IO (Options, [String])
parseArgs argv      = case getOpt Permute optsDescr argv of
                        (xs, ys, []) ->
                            return (foldl (flip ($)) defaultOpts xs, ys)
                        (_, _, errs) ->
                            fail (concat errs ++ usageInfo header optsDescr)
  where header      = "Usage: show_words [OPTION...] columnNames.."

-- FIXME: Change sequence:
--  splitToColumns -> ToPhrases -> reorderColumns.
-- After all, if reordercolumns written correctly, this should have no
-- difference when i call it. But such sequence allows to use new
-- splitToColumns instead of splitToPhrases.
-- Show words (main function).
showWords :: IO ()
showWords = do
    argv <- getArgs
    (Options
          { optMode = mode
          , optLineOrder = lineOrder
          , optFile = file
          , optColumnSep = colSp
          , optPhraseSep = phrSp
          , optReferenceSep = refSp}
      , colNames) <- parseArgs argv
    contents <- readFile' file
    hSetEcho stdin False
    -- FIXME: Applicative (->) with wSps ?
    let wSps = WordsSeps
                { columnSep = colSp
                , phraseSep = phrSp
                , referenceSep = refSp
                }
    xs <- reorderLines lineOrder
            . map (fmap (map dropSpaces))
            . splitToPhrases wSps
            . reorderColumns refEq colNames
            . splitToColumns wSps
            . lines
            $ contents
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


-- New showWords..
showWords0 :: IO ()
showWords0 = do
    (conf, colNames) <- parseArgs1
    -- FIXME: Set confColNames here or in parseArgs?
    runReaderT showWords1 (conf {confColumnNames = colNames})

showWords1 :: ReaderT Config IO ()
showWords1          = do
    contents <- readFile'
    lift (hSetEcho stdin False)
    Config {confColumnNames = colNames} <- ask
    xs  <- splitToPhrases1 <=< splitToColumns1 <=< return . lines $ contents
    xs' <- reorderLines1 . reorderColumns refEq (map (: []) colNames) $ xs
    putPhrases1 
        . dropSpaces'
        $ xs'
    lift (putStrF "Bye!\n")
  where
    readFile' :: ReaderT Config IO String
    readFile'       = do
        Config {confInputFile = file} <- ask
        contents <- lift (B.readFile file)
        return . decode . B.unpack $ contents
    -- Equality test for reference columns.
    refEq :: [String] -> [String] -> Bool
    refEq xs ys     = map dropSpaces xs == map dropSpaces ys
    dropSpaces' :: Functor f => [f [String]] -> [f [String]]
    dropSpaces'     = map (fmap (map dropSpaces))

defaultOpts1 :: Config
defaultOpts1        = Config
                        { confMode         = setMode "default"
                        , confInputFile    = "./words.txt"
                        , confReferenceSep = " - "
                        , confColumnSep    = " : "
                        , confPhraseSep    = "; "
                        , confColumnNames  = []
                        , confLineOrder    = "default"
                        }

optsDescr1 :: [OptDescr (Config -> Config)]
optsDescr1 = 
    [ Option    ['m']
                ["mode"]
                (ReqArg (\mode conf -> conf {confMode = setMode mode}) "MODE")
                ("Set operation mode to MODE (default 'reorder').\n"
                    ++ "Valid values are 'print', 'check', 'reorder'."
                )
    , Option    ['f']
                ["file"]
                (ReqArg (\file conf -> conf {confInputFile = file}) "FILE")
                "Read words from FILE (default 'words.txt')."
    , Option    ['r']
                ["reference-sep"]
                (ReqArg (\refSp conf -> conf {confReferenceSep = refSp})
                        "REFERENCE_SEP"
                )
                "Reference (heading) separator (default \" - \")."
    , Option    ['c']
                ["column-sep"]
                (ReqArg (\colSp conf -> conf {confColumnSep = colSp})
                        "COLUMN_SEP"
                )
                "Column separator (default \" : \")."
    , Option    ['p']
                ["phrase-sep"]
                (ReqArg (\phrSp conf -> conf {confPhraseSep = phrSp})
                        "PHRASE_SEP"
                )
                "Phrase separator (default \"; \")."
    , Option    ['s']
                ["shuffle"]
                (NoArg (\conf -> conf {confLineOrder = "shuffle"}))
                "Shuffle lines (default disabled)."
    ]

-- FIXME: Set colNames here?
-- Parse command-line arguments.
parseArgs1 :: IO (Config, [String])
parseArgs1          = do
    argv <- getArgs
    case getOpt Permute optsDescr1 argv of
      (xs, ys, []) ->
          return (foldl (flip ($)) defaultOpts1 xs, ys)
      (_, _, errs) ->
          fail (concat errs ++ usageInfo header optsDescr)
  where
    header          = "Usage: show_words [OPTION..] columnNames.."

