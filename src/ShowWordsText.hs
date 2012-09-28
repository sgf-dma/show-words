
module ShowWordsText
    ( WordsSeps (..)
    , dropSpaces
    , splitToColumns
    , splitToPhrases
    , reorderColumns
    , reorderLines
    , splitToColumns1
    , splitToPhrases1
    , reorderLines1
    )
  where

import Data.Char (isSpace)
import qualified Data.Foldable as F
-- FIXME: Move random code to ShowWords?
import System.Random (getStdGen, newStdGen)
import Control.Monad.Reader

import SgfList
import SgfOrderedLine
import ShowWordsOptions


-- FIXME: WordSeps is deprecated.
-- Input (and output) separators.
data WordsSeps      = WordsSeps
                        { columnSep    :: String -- Column separator.
                        , phraseSep    :: String -- Phrase separator.
                        , referenceSep :: String -- Heading separaror.
                        }
  deriving (Show)

-- Drop leading and trailing spaces.
dropSpaces :: String -> String
dropSpaces          = dropWhile isSpace . dropWhileEnd isSpace

-- Determine whether string ends at unescaped backslash (escape character is
-- also backslash). Also in _any_ case remove all trailing backslashes.
--
-- FIXME: I'm not sure, whether i should remove all trailing backslashes every
-- time or only last one and only if it is unescaped. After all, only the last
-- unescaped backslash continues line, but others are just characters. On the
-- other hand, they're almost ever a garbage.
isContinued :: String -> (String, Bool)
isContinued         = foldr f ([], False)
  where
    f :: Char -> (String, Bool) -> (String, Bool)
    f x ([], s)
      | x == '\\'   = ([], not s)
      | otherwise   = ([x], s)
    f x (zs, s)     = (x : zs, s)

-- Folding function for foldrMerge. Split input line to columns using splitBy
-- with current separator (tracked in backward state). Then filter out all
-- separators and check whether this input line continues on next line. If so,
-- return True and do not change separator, so the next line will be split in
-- the same way and foldrMerge can mappend them. Otherwise, return False and
-- choose next separator. If only one separator remains, use it for all
-- remaining input lines.
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
    split :: String -> String -> ([String], Bool)
    split k         = foldr (\(x, p) (zx, zp) -> (x : zx, p || zp)) ([], False)
                        . map isContinued
                        . filter (/= k)
                        . splitBy k
    f :: String -> [String] -> (([String], Bool), [String])
    f x []          = (split [] x, [])
    f x (k : ks)
      | null ks     = (z, [k])
      | otherwise   = if p
                        then (z, k : ks)
                        else (z, ks)
      where
        z@(_, p)    = split k x

-- Split input lines (strings) into columns. First line is treated as
-- reference (heading) and referenceSep is used to split it. Other are split
-- by columnSep. If at least one column of a line continues on next line,
-- lines will be merged column by column.
splitToColumns :: WordsSeps -> [String] -> [[String]]
splitToColumns (WordsSeps {referenceSep = refSp, columnSep = colSp})
                    = map getZipList'
                        . fst
                        . flip runBState [refSp, colSp]
                        . foldrMerge splitTextLine

-- Split columns (strings) into phrases. First line is treated as reference,
-- and will not be split into phrases. If this function called after
-- splitToColumns, functor f will be list, if after reorderColumns it'll be
-- Line.
splitToPhrases :: (Functor f) => WordsSeps -> [f String] -> [f [String]]
splitToPhrases _ []     = []
splitToPhrases (WordsSeps {phraseSep = phrSp}) (ref : xs)
                        = fmap (: []) ref : map (fmap split) xs
  where
    split :: String -> [String]
    split               = filter (/= phrSp) . splitBy phrSp

-- Convert list of lines (list of lists) into list of Line-s. This will
-- reorder elements in lines according to supplied new column order. First
-- line treated as reference (heading) and should contain column names in
-- current order. It will also be reordered.
reorderColumns :: (a -> a -> Bool) -> [a] -> [[a]] -> [Line a]
reorderColumns _ _ []   = []
reorderColumns eq colNames xs@(refs : _)
                        = makeRef . map (orderList colOrder) $ xs
  where
    colOrder :: [Index]
    colOrder            = elemsOrder eq colNames refs
    -- I need to store reference as "other" element (see Line description) to
    -- not execute any actions before its columns.
    makeRef :: [Line a] -> [Line a]
    makeRef (refl : ys) = orderList [] (F.foldr (:) [] refl) : ys

-- FIXME: For v3. Add support for cards. Oh.. yeah, this is not going to be
-- easy.  I should add reorderCards. reorderLines should reorder lines in one
-- Card (or all). This also may be implemented more generally: add support for
-- tag column, which have arbitrary number of tags. And define operations with
-- them.
-- FIXME: Rename to "shuffleLines" and remove order argument?
-- FIXME: For v2.1. Shuffle lines according to statistics. Make words with
-- more mistakes having more chances to appear at the beginning. Or i can
-- simply shuffle lines in group with the same error rate, and output groups
-- in error rate decreasing order.
-- Shuffle (or not) lines.
reorderLines :: String -> [a] -> IO [a]
reorderLines _         []   = return []
reorderLines lineOrder xl@(x : xs)
  | lineOrder == "shuffle"  = do
                                gen <- getStdGen
                                _ <- newStdGen
                                return (x : shuffleList gen xs)
  | otherwise               = return xl




splitToColumns1 :: (Monad m) => [String] -> ReaderT Config m [[String]]
splitToColumns1 xs  = do
    Config {confReferenceSep = refSp, confColumnSep = colSp} <- ask
    return
        . map getZipList'
        . fst
        . flip runBState [refSp, colSp]
        . foldrMerge splitTextLine
        $ xs

-- (WordsSeps {phraseSep = phrSp})
-- FIXME: Move [] case to lower level? E.g. in BState ? As in splitToColumns?
-- FIXME: Continuation of phrases with backslash?
splitToPhrases1 :: (Monad m, Functor f) =>
                   [f String] -> ReaderT Config m [f [String]]
splitToPhrases1 []          = return []
splitToPhrases1 (ref : xs)  = do
    Config {confPhraseSep = phrSp} <- ask
    let split = filter (/= phrSp) . splitBy phrSp
    return (fmap (: []) ref : map (fmap split) xs)
    

-- FIXME: Abstract to transformer of monad m? And move all random to
-- ShowWords?
-- FIXME: Rewrite code?
reorderLines1 :: [a] -> ReaderT Config IO [a]
reorderLines1 []     = return []
reorderLines1 xl@(x : xs)   = do
    Config {confLineOrder = lineOrder} <- ask
    if lineOrder == "shuffle"
      then do
            gen <- lift getStdGen
            _ <- lift newStdGen
            return (x : shuffleList gen xs)
      else  return xl

