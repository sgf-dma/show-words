
module ShowWordsText
    ( WordsSeps(..)
    , Column
    , Phrase
    , dropSpaces
    , splitToColumns
    , splitToPhrases
    , orderColumns
    , reorderColumns
    , reorderLines
    )
  where

import Data.Char                -- For isSpace.
import Control.Applicative      -- For Applicative ((->) a), <$> and other.
import System.Random            -- For randomRs.

import SgfList
import SgfOrderedLine

-- FIXME: Should i move splitTo.. and reorder.. function to SgfText? On the
-- one hand, this is not general functions and have nothing common with
-- Data.Test, but on the other.. well, perhaps, it's time to split this file?
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


-- FIXME: These types are confusing: why Column is not [Phrase] ? But, on the
-- other hand, i can't make it [Phrase], because i need exactly String. So,
-- are they useless?
type Sep            = String
type Column         = String
type Phrase         = String
-- Input (and output) separators.
data WordsSeps      = WordsSeps
                        { getColumnSep    :: String -- Column separator.
                        , getPhraseSep    :: String -- Phrase separator.
                        , getReferenceSep :: String -- Heading separaror.
                        }
  deriving (Show)

-- Drop leading and trailing spaces.
dropSpaces :: String -> String
dropSpaces          = dropWhile isSpace . dropWhileEnd isSpace

{-
-- FIXME: Am i need this function?
-- Determine whether string ends at unescaped backslash (escape character is
-- also backslash).
lineCont :: String -> Bool
lineCont            = foldl f False
  where
    f :: Bool -> Char -> Bool
    f s x
      | x == '\\'   = not s
      | otherwise   = False-}

-- Determine whether string ends at unescaped backslash (escape character is
-- also backslash) and in _any_ case remove all trailing backslashes.
--
-- FIXME: I'm not sure, whether i should remove all trailing backslashes every
-- time or only last one and only if it's unescaped. After all, only the last
-- unescaped backslash continues line, but others are just characters.
isContinued :: String -> (String, Bool)
isContinued         = foldr f ([], False)
  where
    f :: Char -> (String, Bool) -> (String, Bool)
    f x ([], s)
      | x == '\\'   = ([], not s)
      | otherwise   = ([x], s)
    f x (zs, s)     = (x : zs, s)

-- Folding function for foldrMerge. Split input line to columns using splitBy
-- with current separator (tracked in state). Then filter out all separators
-- and check whether this input line continues on next line. If so, return
-- True, so foldrMerge can mappend it to next input line.
-- Note, that pattern matching in monadic function will hang Backward State
-- monad, because it will require to evaluate result even for looking what
-- next state will be. So i should either use lazy pattern matching or pattern
-- match in another function, like
--
--      BState (f x) >>= return . wrapInZipList'
--
splitTextLine :: String -> BState [String] (ZipList' String, Bool)
splitTextLine x     = BState (f x) >>= \ ~(xs, p) -> return (ZipList' xs, p)
  where
    --split :: String -> String -> ([String], Bool)
    split k         = foldr (\(x, p) (zx, zp) -> (x : zx, p || zp)) ([], False)
                        . map isContinued
                        . filter (/= k)
                        . splitBy k
    --f :: String -> [String] -> (([String], Bool), [String])
    f x []          = (split [] x, [])
    f x (k : ks)
      | null ks     = (z, [k])
      | otherwise   = if p
                        then (z, k : ks)
                        else (z, ks)
      where
        z@(_, p)    = split k x

-- FIXME: Add description.
--splitToColumns :: Sep -> Sep -> [String] -> [[Column]]
splitToColumns :: WordsSeps -> [String] -> [[String]]
splitToColumns (WordsSeps {getReferenceSep = refSp, getColumnSep = colSp})
                    = map getZipList'
                        . fst
                        . flip runBState [refSp, colSp]
                        . foldrMerge splitTextLine

-- FIXME: Should i rewrite it to use foldrMerge? Really, i see no sense in
-- this, because splitToPhrases will never need to merge lines.
-- Split ordered columns (Line-s) into phrases. First line is treated as
-- reference, and does not split into phrases.
splitToPhrases :: WordsSeps -> [Line Column] -> [Line [Phrase]]
splitToPhrases _     [] = []
splitToPhrases (WordsSeps {getPhraseSep = phrSp}) (ref : xs)
                        = let ref' = mapLine1 (: []) ref
                              xs'  = map (mapLine1 (splitBy phrSp)) xs
                          in  ref' : xs'

-- FIXME: Does this function really belongs to OrderedLine module? Or better
-- to ShowWordsText?
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

-- Order columns (convert to Line-s) according to supplied new column order.
reorderColumns :: [Column] -> WordsSeps -> [[Column]] -> [Line Column]
reorderColumns _        _     []        = []
reorderColumns colNames (WordsSeps {getReferenceSep = refSp}) (ref : xs)
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

-- Shuffle (or not) lines.
reorderLines :: String -> [Line a] -> IO [Line a]
reorderLines _         []   = return []
reorderLines lineOrder xl@(x : xs)
  | lineOrder == "shuffle"  = do
                                gen <- getStdGen
                                _ <- newStdGen
                                return (x : shuffleList gen xs)
  | otherwise               = return xl

