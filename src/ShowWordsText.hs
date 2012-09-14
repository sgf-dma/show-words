
module ShowWordsText
    ( WordsSeps(..)
    , Column
    , Phrase
    , dropSpaces
    , splitToColumns
    , splitToPhrases
    , reorderColumns
    , reorderLines
    )
  where

import Data.Char (isSpace)
import System.Random (getStdGen, newStdGen)
import Control.Applicative      -- For Applicative ((->) a), <$> and other.

import SgfList
import SgfOrderedLine

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


-- FIXME: These types are confusing: why Column is not [Phrase] ? But, on the
-- other hand, i can't make it [Phrase], because i need exactly String. So,
-- are they useless?
type Sep            = String
type Column         = String
type Phrase         = String
type Column'        = [String]
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
-- unescaped backslash continues line, but others are just characters. On the
-- other hand, they're almost ever useless.
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
-- True and do not change separator, so the next line will be split in the
-- same way and foldrMerge can mappend them. Otherwise, return False and
-- choose next separator. If only one separator remains, use it for the rest
-- of text.
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

-- FIXME: Add description.
--splitToColumns :: Sep -> Sep -> [String] -> [[Column]]
splitToColumns :: WordsSeps -> [String] -> [[String]]
splitToColumns (WordsSeps {referenceSep = refSp, columnSep = colSp})
                    = map getZipList'
                        . fst
                        . flip runBState [refSp, colSp]
                        . foldrMerge splitTextLine

-- Split ordered columns (Line-s) into phrases. First line is treated as
-- reference, and does not split into phrases.
splitToPhrases :: (Functor f) => WordsSeps -> [f String] -> [f [String]]
splitToPhrases _ []     = []
splitToPhrases (WordsSeps {phraseSep = phrSp}) (ref : xs)
                        = fmap (: []) ref : map (fmap split) xs
  where
    split :: String -> [String]
    split               = filter (/= phrSp) . splitBy phrSp

{-
splitToPhrases :: WordsSeps -> [Line Column] -> [Line [Phrase]]
splitToPhrases _     [] = []
splitToPhrases (WordsSeps {phraseSep = phrSp}) (ref : xs)
                        = let ref' = mapLine1 (: []) ref
                              xs'  = map (mapLine1 (splitBy phrSp)) xs
                          in  ref' : xs'-}

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
    colOrder            = elemsOrder eq refs colNames
    makeRef :: [Line a] -> [Line a]
    makeRef (refl : ys) = orderList [] (joinLine id id refl) : ys

{-
makeRef :: [Line Column] -> [Line Column]
makeRef []          = []
makeRef (refl : ys) = let ref' = concat $ joinLine id (refSp ++) refl
                      in  orderList [] [ref'] : ys

makeRef :: [Line a] -> [Line a]-}

{-
-- Order columns (convert to Line-s) according to supplied new column order.
reorderColumns :: [Column] -> WordsSeps -> [[Column]] -> [Line Column]
reorderColumns _        _     []        = []
reorderColumns colNames (WordsSeps {referenceSep = refSp}) (ref : xs)
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
-}

-- FIXME: Change order of arguments for elemsOrder and other?
{-
newtype Ref             = Ref {getRef :: String}
  deriving (Show, Eq)
reorderColumns' :: WordsSeps -> [String] -> [[Column']] -> [Line Column']
reorderColumns' _ _ []  = []
reorderColumns' (WordsSeps {referenceSep = refSp}) colNames xs@(ref : _)
                        = makeRef . map (orderList colOrder) $ xs
  where
    colOrder :: [String] -> [Column] -> [Index]
    colOrder :: [Index]
    colOrder            = let ref' = map dropSpaces . concat $ ref
                          in  elemsOrder (==) ref' colNames
    -- I need to join reference into one "other" element (see Line
    -- description) to output it at once without any actions have been
    -- executed before.
    makeRef :: [Line Column'] -> [Line Column']
    makeRef []          = []
    makeRef (refl : ys) = let ref' = concat $ joinLine id (refSp ++) refl
                          in  orderList [] [ref'] : ys-}

-- Shuffle (or not) lines.
reorderLines :: String -> [Line a] -> IO [Line a]
reorderLines _         []   = return []
reorderLines lineOrder xl@(x : xs)
  | lineOrder == "shuffle"  = do
                                gen <- getStdGen
                                _ <- newStdGen
                                return (x : shuffleList gen xs)
  | otherwise               = return xl

