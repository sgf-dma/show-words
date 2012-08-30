
module ShowWords
    ( WordsSeps(..)
    , showWords)
  where

import System.IO                -- For hSetEcho, hFlush, stdin, stdout.
import System.Environment       -- For getArgs.
import Data.Char                -- For isSpace.
import Data.List                -- For deleteFirstBy.
import Control.Applicative      -- For Applicative ((->) a), <$> and other.
import Control.Monad.State      -- For State monad.
import Codec.Binary.UTF8.String -- For encode, decode.
import qualified Data.ByteString.Lazy as B
import System.Random            -- For randomRs.
import System.Console.GetOpt    -- For getOpt.

import SgfListIndex
import SgfOrderedLine

-- FIXME: Tests.
-- FIXME: Use '-' for read words from stdin. But stdin is used for interaction
-- with user.
-- FIXME: Not literal match for separators?
-- FIXME: Diabled echo for "check" mode is not convenient. Though, if it is
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
splitByM _  _  []   = return []
splitByM _  [] xs   = return [xs]
splitByM eq sp xs   = foldrM (\x -> State . f x) [[]] xs
  where
    -- splitByM ensures, that state is not empty list (f itself never makes
    -- state empty, the only possible case is empty initial state) and that
    -- accumulator is not empty list.
    --f :: a -> [[a]] -> [a] -> ([[a]], [a])
    f x (z : zs) [k]
      | x `eq` k    = ([] : deleteFirstsBy eq (x : z) sp : zs, sp)
    f x (z : zs) (k : ks)
      | x `eq` k    = ((x : z) : zs, ks)
      | otherwise   = ((x : z) : zs, sp)

-- Split string by supplied character (omitting separator itself) and remove
-- leading and trailing spaces from each substring (inner spaces preserved).
splitStrBy :: String -> String -> [String]
splitStrBy sep      = map dropSpaces . splitBy (==) sep
  where
    dropSpaces :: String -> String
    dropSpaces      = dropWhile isSpace . dropWhileEnd isSpace

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
        elem' k     = foldr (\y z -> if y `eq` k then True else z) False

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

-- Split input lines into columns, order columns (convert to Line-s) according
-- to supplied new column order and split columns to phrases.
reorderColumns :: [Column] -> WordsSeps -> [String] -> [Line [Phrase]]
reorderColumns colNames (WordsSeps { columnSep = sp
                                   , phraseSep = psp
                                   , referenceSep = rsp})
                    = splitToPhrases
                        . makeRef
                        . orderColumns isPrefixOf colNames -- :: -> [Line Column]
                        . splitToColumns                   -- :: -> [[Column]]
  where
    splitToColumns :: [String] -> [[Column]]
    splitToColumns []           = []
    splitToColumns (ref : xs)   = splitStrBy rsp ref : map (splitStrBy sp) xs
    makeRef :: [Line Column] -> [Line Column]
    makeRef []          = []
    makeRef (refl : xs) = let ref' = concat $ joinLine id (rsp ++) refl
                          in  (orderList [] [ref']) : xs
    splitToPhrases :: [Line Column] -> [Line [Phrase]]
    splitToPhrases  []  = []
    splitToPhrases (ref : xs)
                    = mapLine1 (: []) ref : map (mapLine1 (splitStrBy psp)) xs

-- Shuffle lines.
reorderLines :: [Line [Phrase]] -> IO [Line [Phrase]]
reorderLines []     = return []
reorderLines (x : xs)
                    = do
                        gen <- getStdGen
                        _ <- newStdGen
                        return (x : shuffleList gen xs)


putStrF :: String -> IO ()
putStrF x           = do
                        B.putStr $ B.pack $ encode x
                        hFlush stdout

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
putPhrases :: (Phrase -> IO Phrase) -> WordsSeps -> [Line [Phrase]] -> IO ()
putPhrases f (WordsSeps {columnSep = sp, phraseSep = psp})
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
        joinPhrases (mx : mxs)  = mx : map ((psp ++) <$>) mxs


-- Usage: ./show_words mode file [column_names]
--
--   Show words (phrases) from file one by one in specified column order and
-- check your answers against next word.
--
--      'mode' - operation mode:
--          - "print" for waiting for a key after each phrase in specified
--            columns,
--          - "check" for checking user input against each next phrase in
--            specified columns (literal check).
--      'file' - file with words.
--      [column_names] - any number of any column names (may be prefixes), one
--                       in one command line argument.
--
--   File must contain lines of words. Line may contain several columns
-- separated by columnSep. Column may contain several phrases separated by
-- phraseSep.  First line of file treated as heading (reference), and should
-- contain column names in current order separated by referenceSep.
--
--   File will be displayed line by line in the specified columns order with
-- leading and trailing spaces of each column and each phrase deleted (inner
-- spaces preserved). You may specify desired column order at command line -
-- any column names (or just prefixes of column names) in any order.
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
-- double check column names in words file and on cmd!

-- Set appropriate operation mode (how to put phrases) from string.
setMode :: String -> (String -> IO String)
setMode xs
  | xs == "check"   = checkAnswer
  | xs == "print"   = waitKey
  | otherwise       = return

data Options        = Options
                        { optMode    :: String -> IO String
                        , optShuffle :: [Line [Phrase]] -> IO [Line [Phrase]]
                        , optFile    :: FilePath
                        }

defaultOpts :: Options
defaultOpts         = Options
                        { optMode    = setMode "default"
                        , optShuffle = return
                        , optFile    = "./words.txt"
                        }

optsDescr :: [OptDescr (Options -> Options)]
optsDescr = 
    [ Option    ['m']
                ["mode"]
                (ReqArg (\xs opt -> opt {optMode = setMode xs}) "MODE")
                "Set operation mode to MODE."
    , Option    ['s']
                ["shuffle"]
                (NoArg (\opt -> opt {optShuffle = reorderLines}))
                "Shuffle lines."
    , Option    ['f']
                ["file"]
                (ReqArg (\xs opt -> opt {optFile = xs}) "FILE")
                "Read words from FILE (words.txt by default)."
    ]

-- Parse command-line arguments.
parseArgs :: [String] -> IO (Options, [String])
parseArgs argv      = do
    case getOpt Permute optsDescr argv of
      (xs, ys, [])  -> return (foldl (\z f -> f z) defaultOpts xs, ys)
      (_, _, errs)  -> fail (concat errs ++ usageInfo header optsDescr)
  where header      = "Usage: show_words [OPTION...] columnNames.."

-- Show words (main function).
showWords :: WordsSeps -> IO ()
showWords wsp       = do
    argv <- getArgs
    (Options
          { optMode = mode
          , optShuffle = shuffle
          , optFile = file}
      , colNames) <- parseArgs argv
    contents <- readFile' file
    hSetEcho stdin False
    xs <- shuffle $ reorderColumns colNames wsp $ lines contents
    putPhrases mode wsp xs
    putStrF "Bye!\n"
  where
    readFile' :: FilePath -> IO String
    readFile' file  = do
        contents <- B.readFile file
        return $ decode $ B.unpack contents

