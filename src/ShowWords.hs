
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
import Data.List                -- For deleteFirstBy.
import Control.Applicative      -- For Applicative ((->) a), <$> and other.
import Control.Monad.State      -- For State monad.
import System.Random            -- For randomRs.

import SgfListIndex
import SgfOrderedLine

-- FIXME: Multi-line output. Backsalsh as continuation.
-- FIXME: Escape backslash with backslash.
-- FIXME: Several output formats. Increase number of lines (on which one input
-- line split).
-- FIXME: Empty answer == skip answer, but do not check.
-- FIXME: Tests.
-- FIXME: I disabled prefix match for column names during testing.
-- FIXME: Use '-' for read words from stdin. But stdin is used for interaction
-- with user..
-- FIXME: Not literal match for separators?
-- FIXME: Disabled echo for "check" mode is not convenient. Though, if it is
-- enabled, newline will break all output.


-- On my system Data.List does not contain dropWhileEnd.
dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd p      = foldr (\x z -> if null z && p x then [] else x : z) []

-- Split list by list (separator is list of elements), omitting separator
-- itself, Note, that this function can't be implemented using Backward State
-- monad (i think).
splitBy :: (a -> a -> Bool) -> [a] -> [a] -> [[a]]
splitBy eq sp xs   = let sp' = reverse sp
                     in  fst $ runState (splitByM eq sp' xs) sp'

splitByM :: (a -> a -> Bool) -> [a] -> [a] -> State [a] [[a]]
splitByM _  _  []           = return []
splitByM _  [] xs           = return [xs]
splitByM eq sp@(k : ks) xs  = foldrM (\x -> State . f x) [[]] xs
  where
    -- splitByM ensures, that state is not empty list (f itself never makes
    -- state empty, the only possible case is empty initial state) and that
    -- accumulator is not empty list.
    --f :: a -> [[a]] -> [a] -> ([[a]], [a])
    f x (z : zs) [c]
      | x `eq` c    = ([] : deleteFirstsBy eq (x : z) sp : zs, sp)
    f x (z : zs) (c : cs)
      | x `eq` c    = ((x : z) : zs, cs)
      | x `eq` k    = ((x : z) : zs, ks)
      | otherwise   = ((x : z) : zs, sp)

dropSpaces :: String -> String
dropSpaces          = dropWhile isSpace . dropWhileEnd isSpace

-- Pick from a list xs first occurences of all elements found in reference
-- list ks.  Stop processing a list xs if all reference elements have found.
-- Works with inifinity list xs, if it contain all elements from reference
-- list ks.  May be used to make random transposition from randomRs output.
transp :: (a -> a -> Bool) -> [a] -> [a] -> [a]
transp eq ks xs     = fst $ runBState (transpM eq xs) ks

transpM :: (a -> a -> Bool) -> [a] -> BState [a] [a]
transpM eq          = foldrM (\x zs -> BState $ f x zs) []
  where
    --f :: a -> [a] -> [a] -> ([a], [a])
    f _ _  []           = ([], [])
    f x zs ks
      | x `elem'` ks    = (x : zs, filter (not . (`eq` x)) ks)
      | otherwise       = (zs, ks)
      where
        --elem' :: a -> [a] -> Bool
        elem' k     = foldr (\y z -> (y `eq` k) || z) False

-- Shuffle list elements.
shuffleList :: RandomGen g => g -> [a] -> [a]
shuffleList g xs    = let lx = length xs
                          ts = transp (==) (take lx [1..]) $ randomRs (1, lx) g
                      in  ts >>= elemByInd xs


type Column         = String
type Phrase         = String
-- Input (and output) separators.
data WordsSeps      = WordsSeps
                        { columnSep     :: String   -- Column separator.
                        , phraseSep     :: String   -- Phrase separator.
                        , referenceSep  :: String   -- Heading separaror.
                        }
  deriving (Show)

-- Split input lines (strings) into columns. First line is treated as
-- reference (heading) and referenceSep is used to split it. Other are split
-- by columnSep.
splitToColumns :: String -> String -> [String] -> [[Column]]
splitToColumns _     _     []           = []
splitToColumns refSp colSp (ref : xs)
                    = let ref'  = splitBy (==) refSp ref
                          xs'   = map (splitBy (==) colSp) xs
                      in  ref' : xs'

-- Order columns (convert to Line-s) according to supplied new column order.
reorderColumns :: [Column] -> String -> [[Column]] -> [Line Column]
reorderColumns _        _     []        = []
reorderColumns colNames refSp (ref : xs)
                        = let ref' = map dropSpaces ref
                          in  makeRef $ orderColumns (==) colNames (ref' : xs)
  where
    -- I need to join reference into one "other" element (see Line
    -- description) to output it at once without any actions have been
    -- executed before.
    makeRef :: [Line Column] -> [Line Column]
    makeRef []          = []
    makeRef (refl : ys) = let ref' = concat $ joinLine id (refSp ++) refl
                          in  orderList [] [ref'] : ys

-- Split ordered columns (Line-s) into phrases. First line is treated as
-- reference, and does not split into phrases.
splitToPhrases :: String -> [Line Column] -> [Line [Phrase]]
splitToPhrases _     [] = []
splitToPhrases phrSp (ref : xs)
                        = let ref' = mapLine1 (: []) ref
                              xs'  = map (mapLine1 (splitBy (==) phrSp)) xs
                          in  ref' : xs'

-- Shuffle (or not) lines.
reorderLines :: String -> [Line a] -> IO [Line a]
reorderLines _         []   = return []
reorderLines lineOrder xl@(x : xs)
  | lineOrder == "shuffle"  = do
                                gen <- getStdGen
                                _ <- newStdGen
                                return (x : shuffleList gen xs)
  | otherwise               = return xl


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
putPhrases :: (Phrase -> IO Phrase) -> String -> String -> [Line [Phrase]] -> IO ()
putPhrases f colSp phrSp
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

-- Show words (main function).
showWords :: WordsSeps -> IO ()
showWords (WordsSeps
             { columnSep = colSp
             , phraseSep = phrSp
             , referenceSep = refSp}) = do
    argv <- getArgs
    (Options
          { optMode = mode
          , optLineOrder = lineOrder
          , optFile = file}
      , colNames) <- parseArgs argv
    contents <- readFile' file
    hSetEcho stdin False
    xs <- reorderLines lineOrder
            $ map (mapLine1 (map dropSpaces))
            $ splitToPhrases phrSp
            $ reorderColumns colNames refSp
            $ splitToColumns refSp colSp
            $ lines contents
    putPhrases mode colSp phrSp xs
    putStrF "Bye!\n"
  where
    readFile' :: FilePath -> IO String
    readFile' file  = do
        contents <- B.readFile file
        return $ decode $ B.unpack contents

