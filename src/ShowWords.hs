
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
import Codec.Binary.UTF8.String -- For encode.
import qualified Data.ByteString.Lazy as B

import SgfListIndex
import SgfOrderedLine

-- FIXME: Makefile
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

showWords :: WordsSeps -> IO ()
showWords wsp       = do
    args <- getArgs
    (mode : file : colNames) <- parseArgs args
    contents <- readFile' file
    hSetEcho stdin False
    putPhrases (setMode mode) wsp
        $ reorderColumns colNames wsp
        $ lines contents
    putStrF "Bye!\n"
  where
    parseArgs :: [String] -> IO [String]
    parseArgs xs@(_ : _ : _)    = return xs
    parseArgs _                 = fail $ "Too few arguments. "
                                    ++ "Usage: ./show_words mode file [column_names]"
    readFile' :: FilePath -> IO String
    readFile' file  = do
        contents <- B.readFile file
        return $ decode $ B.unpack contents
    setMode :: String -> (String -> IO String)
    setMode xs
      | xs == "check"   = checkAnswer
      | xs == "print"   = waitKey
      | otherwise       = return

