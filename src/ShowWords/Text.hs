
module ShowWords.Text
    ( dropSpaces
    , splitToColumns
    , splitToPhrases
    , reorderColumns
    , reorderLines
    )
  where

import Data.Char (isSpace)
import qualified Data.Foldable as F
import System.Random (RandomGen)
import Control.Monad.Reader

import Sgf.List
import Sgf.OrderedLine
import ShowWords.Config (Config (..))


-- Drop leading and trailing spaces.
dropSpaces :: String -> String
dropSpaces          = dropWhile isSpace . dropWhileEnd isSpace

-- Determine whether string ends at unescaped backslash (escape character is
-- also backslash). Also in _any_ case remove all trailing backslashes.
--
-- FIXME: I'm not sure, whether i should remove all trailing backslashes every
-- time or only last one and only if it is unescaped. After all, only the last
-- unescaped backslash continues line, but others are just characters. On the
-- other hand, they're almost ever just a garbage.
hasContinue :: String -> (String, Bool)
hasContinue         = foldr f ([], False)
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
    split k         = foldr hasContinue' ([], False)
                        . filter (/= k)
                        . splitBy k
      where
        hasContinue' x (zx, zp)
                    = let (x', p) = hasContinue x in (x' : zx, p || zp)
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
splitToColumns :: (Monad m) => [String] -> ReaderT Config m [[String]]
splitToColumns xs   = do
    Config {confReferenceSep = refSp, confColumnSep = colSp} <- ask
    let split = map getZipList'
                    . fst
                    . flip runBState [refSp, colSp]
                    . foldrMerge splitTextLine
    return (split xs)

-- FIXME: Continue phrases with backslash as well?
-- Split columns (strings) into phrases. First line is treated as reference,
-- and will not be split into phrases. If this function called after
-- splitToColumns, functor f will be list, if after reorderColumns it'll be
-- Line.
splitToPhrases :: (Monad m, Functor f) =>
                  [f String] -> ReaderT Config m [f [String]]
splitToPhrases []           = return []
splitToPhrases (ref : xs)   = do
    Config {confPhraseSep = phrSp} <- ask
    let split = filter (/= phrSp) . splitBy phrSp
    return (fmap (: []) ref : map (fmap split) xs)

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
    makeRef []          = []
    makeRef (refl : ys) = orderList [] (F.foldr (:) [] refl) : ys

-- FIXME: For v3. Add support for tag column, which have arbitrary number of
-- tags. And define operations with them.
-- FIXME: Rename to "shuffleLines" and remove order argument?
-- FIXME: For v2.1. Shuffle lines according to statistics. Make words with
-- more mistakes having more chances to appear at the beginning. Or i can
-- simply shuffle lines in group with the same error rate, and output groups
-- in error rate decreasing order.
-- Shuffle (or not) lines.
reorderLines :: (RandomGen g, Monad m) => g -> [a] -> ReaderT Config m [a]
reorderLines _   []         = return []
reorderLines gen (x : xs)   = do
    Config {confLineOrder = lineOrder} <- ask
    if lineOrder == "shuffle"
      then return (x : shuffleList gen xs)
      else return (x : xs)

