
module ShowWords
    ( showWords0
    )
  where

import System.IO                -- For hSetEcho, hFlush, stdin, stdout.
import Codec.Binary.UTF8.String -- For encode, decode.
import qualified Data.ByteString.Lazy as B
import System.Environment       -- For getArgs.
import System.Console.GetOpt    -- For getOpt.
import System.Random (getStdGen)
import Control.Monad.Reader

import SgfList (listEq)
import ShowWordsConfig
import ShowWordsText
import ShowWordsOutput

-- FIXME: v2.1. Another mode, where all phrases outputed at once and user
-- answer checked against them _without_ order. Check for this mode may be
-- implemented with indsByElems with Maybe as Alternative. But this require
-- indsByElems to be rewritten to use Alternative-s..
-- FIXME: v2.2. Add statistic? WriterT w IO, is it not?
-- FIXME: Move defaultConf to ShowWordsConfig? But setAction is not available
-- there..
-- FIXME: Mark place, where column match (literal or not) performed.
-- Also, mark place, where separator matches preformed, as well as answer
-- matches.
-- FIXME: Several output formats. Increase number of lines (on which one input
-- line split):
--      - line by line (current);
--      - column by line;
--      - phrase by line;
-- FIXME: Empty answer == skip answer, but do not check.
-- FIXME: Quickcheck tests,
-- FIXME: Prefix match for column names?
-- FIXME: Use '-' for read words from stdin. But stdin is used for interaction
-- with user, so this is only possible in "reorder" mode.
-- FIXME: Disabled echo for "check" mode is not convenient. Though, if it is
-- enabled, newline will break output formatting.


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

defaultConf :: Config
defaultConf         = Config
                        { confAction       = return
                        , confInputFile    = "./words.txt"
                        , confReferenceSep = " - "
                        , confColumnSep    = " : "
                        , confPhraseSep    = "; "
                        , confColumnEq     = (==)
                        , confColumnNames  = []
                        , confLineOrder    = "disabled"
                        }

-- Set action, which will be executed before every phrase.
setAction :: String -> String -> IO String
setAction xs
  | xs == "check"   = checkAnswer
  | xs == "wait"    = waitKey
  | otherwise       = return

optsDescr :: [OptDescr (Config -> Config)]
optsDescr = 
    [ Option    ['a']
                ["action"]
                (ReqArg (\mode conf -> conf {confAction = setAction mode})
                        "ACTION"
                )
                ("Set action to execute before every phrase to ACTION"
                    ++ "(default is no action). "
                    ++ "Valid values are 'wait' and 'check'."
                )
    , Option    ['f']
                ["file"]
                (ReqArg (\file conf -> conf {confInputFile = file}) "FILE")
                ("Read words from FILE (default \""
                    ++ confInputFile defaultConf
                    ++ "\")."
                )
    , Option    ['r']
                ["reference-sep"]
                (ReqArg (\refSp conf -> conf {confReferenceSep = refSp})
                        "REFERENCE_SEP"
                )
                ("Reference (heading) separator (default \""
                    ++ confReferenceSep defaultConf
                    ++ "\")."
                )
    , Option    ['c']
                ["column-sep"]
                (ReqArg (\colSp conf -> conf {confColumnSep = colSp})
                        "COLUMN_SEP"
                )
                ("Column separator (default \""
                    ++ confColumnSep defaultConf
                    ++ "\")."
                )
    , Option    ['p']
                ["phrase-sep"]
                (ReqArg (\phrSp conf -> conf {confPhraseSep = phrSp})
                        "PHRASE_SEP"
                )
                ("Phrase separator (default \""
                    ++ confPhraseSep defaultConf
                    ++ "\")."
                )
    , Option    ['s']
                ["shuffle"]
                (NoArg (\conf -> conf {confLineOrder = "shuffle"}))
                ("Shuffle lines (default \""
                    ++ confLineOrder defaultConf
                    ++ "\")."
                )
    ]

-- Parse command-line arguments.
parseArgs :: IO (Config, [String])
parseArgs           = do
    argv <- getArgs
    case getOpt Permute optsDescr argv of
      (xs, ys, []) -> return (foldl (flip ($)) defaultConf xs, ys)
      (_, _, errs) -> fail (concat errs ++ usageInfo header optsDescr)
  where
    header          = "Usage: show_words [OPTION..] columnNames.."

-- FIXME: Rename this bootstrap function?
showWords0 :: IO ()
showWords0 = do
    (conf, colNames) <- parseArgs
    runReaderT showWords (conf {confColumnNames = colNames})

showWords :: ReaderT Config IO ()
showWords          = do
    contents <- readFile'
    xs  <- splitToPhrases
            <=< splitToColumns
            <=< return . lines
            $ contents
    colNames <- getColNames
    colEq <- getColEq
    gen <- lift getStdGen
    xs' <- reorderLines gen
            . reorderColumns colEq colNames
            $ xs
    lift (hSetEcho stdin False)
    putPhrases
        . dropSpaces'
        $ xs'
    lift (putStrF "Bye!\n")
  where
    readFile' :: ReaderT Config IO String
    readFile'       = do
        Config {confInputFile = file} <- ask
        contents <- lift (B.readFile file)
        return . decode . B.unpack $ contents
    -- Wrap column names into list, which is required, if i use reorderColumns
    -- after splitToPhrases.
    getColNames :: (Monad m) => ReaderT Config m [[String]]
    getColNames     = do
        Config {confColumnNames = colNames} <- ask
        return $ map (: []) colNames
    -- "Lift" column's equality function to equality for lists, which is
    -- required, if i use reorderColumns after splitToPhrases (and column
    -- names are wrapped into list by getColNames).
    getColEq :: (Monad m) => ReaderT Config m ([String] -> [String] -> Bool)
    getColEq        = do
        Config {confColumnEq = eq} <- ask
        return $ \xs ys ->
            let xs' = map dropSpaces xs
                ys' = map dropSpaces ys
            in  listEq eq xs' ys'
    -- Drop leading and trailing spaces in each phrase.
    dropSpaces' :: Functor f => [f [String]] -> [f [String]]
    dropSpaces'     = map (fmap (map dropSpaces))

