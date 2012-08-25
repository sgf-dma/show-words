
import System.IO                -- For hX
import System.Environment       -- For getArgs
import Data.List                -- For groupBy
import Data.Char                -- For isSpace
import Data.Maybe               -- For fromJust
import Data.Monoid
import Control.Monad
import Control.Applicative      -- For Applicative ((->) a).
import Control.Monad.State      -- For State monad.
import Control.Monad.Reader
import Control.Exception        -- For bracket.
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as B

-- Reimplement some library functions :-)

-- Monadic folds.
foldrM :: (Monad m) => (a -> b -> m b) -> b -> [a] -> m b
foldrM g z []       =  return z
foldrM g z (x : xs) =  foldrM g z xs >>= g x
foldlM              :: (Monad m) => (b -> a -> m b) -> b -> [a] -> m b
foldlM g z []       =  return z
foldlM g z (x : xs) =  g z x >>= \z' -> foldlM g z' xs

-- On my system Data.List does not contain dropWhileEnd.
dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd p      = foldr (\x z -> if null z && p x then [] else x : z) []

-- Start index (for State monads).
indBase             = 1

-- Split list. {{{

-- Reverse of splitAt (count elements from list end).
splitAtEndM :: Index -> [a] -> State Index ([a], [a])
splitAtEndM i xs    = foldrM go ([], []) xs
  where
    go :: a -> ([a], [a]) -> State Index ([a], [a])
    go x (z1, z2)       = State $ \s -> let z' = if s <= i then (z1, x : z2)
                                                   else         (x : z1, z2)
                                        in  (z', s + 1)

-- Unwrap State monad.
splitAtEnd :: Index -> [a] -> ([a], [a])
splitAtEnd i xs         = fst $ runState (splitAtEndM i xs) indBase

-- FIXME: Implement monadic splitBy, which can track State in supplied eq, and
-- hence split by strings.

-- Split list by separator (omitting separator itself).
splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy p []        = []
splitBy p xs        = foldr go [[]] xs
  where
    go x zl@(z : zs)
      | p x         = [] : zl
      | otherwise   = (x : z) : zs

-- END Split list. }}}
-- Index list. {{{

type Index          =  Int
-- Backward state monad from "The essence of functional programming" by Philip
-- Wadler.
newtype BState s a  =  BState {runBState :: (s -> (a, s))}
instance Monad (BState s) where
    return x        =  BState (\s -> (x, s))
    BState m >>= f  =  BState $ \s2 ->
                        let (x, s0)   = m s1
                            BState m' = f x
                            (x', s1)  = m' s2
                        in  (x', s0)

-- Folding functions for use in State monads for indexing list. {{{
-- 
-- Add list element x to the accumulator z only if its index s equals to i.
onlyInds :: [Index] -> a -> [a] -> Index -> ([a], Index)
onlyInds js x z     = \s -> let z' = if s `elem` js then x : z else z
                            in  (z', s + 1)

-- Complement to onlyInd. Add each list elements x to accumulator z if its
-- index s not equals to i.
notInds :: [Index] -> a -> [a] -> Index -> ([a], Index)
notInds js x z      = \s -> let z' = if s `notElem` js then x : z else z
                            in  (z', s + 1)

indsOfs :: (a -> [a] -> Bool) -> [a] -> a -> [Index] -> Index -> ([Index], Index)
indsOfs elem' ks x z  = \s -> let z' = if x `elem'` ks then s : z else z
                              in  (z', s + 1)

-- END Folding functions for use in State monads for indexing list. }}}
-- Index list by right folding it inside Backward State monad. {{{

-- Choose all elements from list which indexes are in the specified index
-- list.
elemsByIndsM :: [Index] -> [a] -> BState Index [a]
elemsByIndsM js     = foldrM (\x -> BState . onlyInds js x) []

-- Complement to elemsByInds. Choose all elements from list which indexes are
-- not in the sepcified index list.
elemsByNotIndsM :: [Index] -> [a] -> BState Index [a]
elemsByNotIndsM js  = foldrM (\x -> BState . notInds js x) []

-- Reverse of elemsByInds. Choose all indexes, which elements from the
-- specified list have.
indsByElemsM :: (a -> a -> Bool) -> [a] -> [a] -> BState Index [Index]
indsByElemsM eq ks  = foldrM (\x -> BState . indsOfs elem' ks x) []
  where elem' y     = foldr (\x z -> if x `eq` y then True else z) False

-- END Index list by folding inside Backward State monad. }}}
-- Unwrap monad from list indexing functions. {{{

elemsByInds :: [a] -> [Index] -> [a]
elemsByInds xs js       = fst $ runBState (elemsByIndsM js xs) indBase

elemsByNotInds :: [a] -> [Index] -> [a]
elemsByNotInds xs js    = fst $ runBState (elemsByNotIndsM js xs) indBase

indsByElems :: (a -> a -> Bool) -> [a] -> [a] -> [Index]
indsByElems eq xs ks    = fst $ runBState (indsByElemsM eq ks xs) indBase

elemByInd :: [a] -> Index -> [a]
elemByInd xs j          = elemsByInds xs [j]

indsByElem :: (a -> a -> Bool) -> [a] -> a -> [Index]
indsByElem eq xs k      = indsByElems eq xs [k]

-- END Unwrap monad from list index functions. }}}

-- END Index list. }}}

-- FIXME: Use string in inPhrSep.
data PhrSep         = PhrSep { inPhraseSep   :: Char
                             , outPhraseSep  :: String
                             , referenceSep  :: String
                             }
  deriving (Show)

-- First list contains ordered elements, second - other.
data Line a         = Line [a] [a]
  deriving (Show)

-- Phrase separators.
phrSep              = '-'
-- Separator for heading.
refSep              = ' ' : phrSep : " "
outPhrSep           = " : "

-- Inline separators into strings (add into string itself, hence no new
-- strings is added to the list). Prepend supplied separator to all strings,
-- except first. 
inlineSeps :: String -> [String] -> [String]
inlineSeps _   []           = []
inlineSeps sep (xa : xs)    = xa : (foldl go ([] ++) xs $ [])
  where
    -- Fast implementation of left-associative (++) expression. Inspired by
    -- DiffList newtype from LYaH.
    go :: ([String] -> [String]) -> String -> [String] -> [String]
    go z x                  = let x' = sep ++ x in z . ([x'] ++)

-- Do not inline separators in empty strings.
-- FIXME: Can i drop first version (inlineSeps) and use this everywhere?
inlineSepsNE :: [String] -> [String]
inlineSepsNE []         = []
inlineSepsNE (xa : xs)  = xa : (foldl go ([] ++) xs $ [])
  where
    go :: ([String] -> [String]) -> String -> [String] -> [String]
    go z []         = z . ([""] ++)
    go z x          = let x' = outPhrSep ++ x in z . ([x'] ++)

-- Convert list of column names to list of column numbers (preserving order).
-- Use supplied reference string for recognizing column names.
phraseOrder :: [String] -> [String] -> [Index]
phraseOrder refs colNames   = colNames >>= indsByElem (==) refs

-- Reorder phrases in the line according to supplied order. All "other"
-- phrases (not referenced from phrase order) will be joined (using inlSeps
-- function) into one last phrase. inlSeps function must add separators into
-- strings (i.e. without adding new list elements), except for the first
-- string in the list.
orderLine :: [Index] -> ([String] -> [String]) -> [String] -> [String]
orderLine phrOrder inlSeps  = makeLine <$> orderedPhrases <*> otherPhrases
  where
    orderedPhrases :: [String] -> [String]
    orderedPhrases ps   = phrOrder >>= elemByInd ps
    otherPhrases :: [String] -> [String]
    otherPhrases   ps   = elemsByNotInds ps phrOrder
    -- If there is ordered phrases (referenced from supplied order), other
    -- phrases string must start with separator (if not empty) to distinguish
    -- "no other phrases" and "one empty other phrase" cases.
    makeLine :: [String] -> [String] -> [String]
    makeLine []  ps2    = let ps2' = (concat $ inlSeps       ps2)  : [] in ps2'
    makeLine ps1 ps2    = let ps2' = (concat $ inlSeps ("" : ps2)) : []
                          in  ps1 ++ ps2'

-- FIXME: Heading is broken by recent orderLine change. Now to inline seps
-- into orderLine result i need to (splitAtEnd indBase) it first.
-- FIXME: First map (orderLine) over all lines, including heading, and then
-- fix heading. Will it be better?

-- Reorder phrases in each line according to column names list.  Not specified
-- columns will be joined into one last phrase at each line. First line is
-- treated as "reference", i.e. containing column names in current order.
-- It'll be also reordered to new order.
reorderPhrases :: [String] -> PhrSep -> [[String]] -> [[String]]
reorderPhrases colNames sep (refs : lss) =
    let lss'    = map (orderLine phrOrder inlPhrSeps) lss
    in  (newRefs : lss')
  where
    inlRefSeps :: [String] -> [String]
    inlRefSeps  = inlineSeps (referenceSep sep)
    inlPhrSeps :: [String] -> [String]
    inlPhrSeps  = inlineSeps (outPhraseSep sep)
    phrOrder :: [Index]
    phrOrder    = phraseOrder refs colNames
    newRefs :: [String]
    newRefs     = let refs'     = orderLine phrOrder inlRefSeps refs
                      (rs, rb)  = splitAtEnd indBase refs'
                  in  (concat (inlRefSeps rs ++ rb)) : []
reorderPhrases _ _ _    = [[]]

-- New interface ;-)
elemsOrder :: (a -> a -> Bool) -> [a] -> [a] -> [Index]
elemsOrder eq xs ks = ks >>= indsByElem eq xs

orderLine1 :: [Index] -> [a] -> Line a
orderLine1 order    = Line  <$> (\xs -> order >>= elemByInd xs)
                            <*> (\xs -> elemsByNotInds xs order)

-- f is some function, which should be applied to only ordered elements.  g is
-- "joining" function. It is applied to all (ordered and others) elements,
-- except first. E.g. it may add spaces to strings to make result suitable for
-- concat.
joinLine :: (a -> a) -> (a -> a) -> Line a -> [a]
joinLine _ _ (Line []            [] ) = []
joinLine _ g (Line []       (y : ys)) = y : map g ys
joinLine f g (Line (x : xs)      ys ) = f x : (map (g . f) xs ++ map g ys)

joinLineM :: (Monad m) => (a -> m a) -> (a -> m a) -> Line a -> [m a]
joinLineM _ _ (Line [] [])       = []
joinLineM _ g (Line [] (y : ys)) = return y : map g ys
joinLineM f g (Line (x : xs) ys) = f x : (map (g <=< f) xs ++ map g ys)

mapLine :: (a -> b) -> Line a -> Line b
mapLine f (Line xs ys)  = Line (map f xs) (map f ys)

reorderPhrases1 :: [String] -> PhrSep -> [[String]] -> [Line String]
reorderPhrases1 colNames (PhrSep {referenceSep = rsp}) (refs : lss) =
    let lss'    = map (orderLine1 phrOrder) lss
        refs'   = joinLine id (rsp ++) $ orderLine1 phrOrder refs
    in  ((Line [] refs') : lss')
  where
    phrOrder :: [Index]
    phrOrder    = elemsOrder (==) refs colNames

-- FIXME: Store line in reverse order. This will require one additional
-- reverse before use, but speed up all other decompositions by splitAtEnd.

-- FIXME: Split by string, not just character.
-- Split each string to phrases by supplied character (omitting separator
-- itself) and remove leading and trailing spaces from each phrase (inner
-- spaces preserved).
splitToPhrases :: Char -> String -> [String]
splitToPhrases sep  = map dropSpaces . splitBy (== sep)
  where
    dropSpaces :: String -> String
    dropSpaces      = dropWhile isSpace . dropWhileEnd isSpace

putStrF :: String -> IO ()
putStrF x           = putStr x >> hFlush stdout

-- Wait for a key from user.
waitKey :: String -> IO String
waitKey _           = getChar >> return ""

-- Check that user entered correct phrase.
checkAnswer :: String -> IO String
checkAnswer []      = return ""
checkAnswer p       = do
                        r <- getLine
                        return (checkPhrase r)
  where
    checkPhrase :: String -> String
    checkPhrase r
      | r == p      = " Ura! "
      | otherwise   = " Ops! "

-- If i inline separators to all phrases, including empty, this
--      - reveals non-existent last element ("other" phrases), when it's
--      empty.
-- If i inline separators before putLine, this
--      - breaks checkAnswer check for empty phrase for do not asking user, if
--      it has no key.
--      - causes checkAnswer's answer ("ops" or "ura") to appear in the
--      previous column instead of in the same column as word it checked
--      against.
-- If i inline separators, skipping empty elements, this
--      - does not preserve column position in output (in case of previous
--      columns was empty).

-- Output phrases and execute specified action before every phrase in a line,
-- except first. First is omitted, because it may be treated as question.
putPhrases :: (String -> IO String) -> [[String]] -> IO ()
putPhrases f            = mapM_ putLine
  where
    putLine :: [String] -> IO ()
    putLine []          = return ()
    putLine [xa]
      | null xa         = return ()
      | otherwise       = putStrF (xa ++ "\n")
    putLine (xa : xs)   = do
                            putStrF xa
                            let (xs', [xb]) = splitAtEnd indBase xs
                            mapM_ (\x -> do
                                r <- f x
                                putStrF (outPhrSep ++ r ++ x)) xs'
                            putStrF (xb ++ "\n")

waitKey1 :: String -> IO String
waitKey1 p          = getChar >> return p

checkAnswer1 :: String -> IO String
checkAnswer1 []     = return ""
checkAnswer1 p      = do
                       r <- getLine
                       return (checkPhrase r ++ p)
  where
    checkPhrase :: String -> String
    checkPhrase r
      | r == p      = " Ura! "
      | otherwise   = " Ops! "

putPhrases1 :: (String -> IO String) -> [Line String] -> IO ()
putPhrases1 f       = mapM_ (\x -> sequence (putLine x) >> putStrF "\n")
  where
    putLine :: Line String -> [IO ()]
    putLine         = map (\mx -> mx >>= putStrF) . joinLineM waitKey1 (return . (" : " ++))

-- FIXME: splitAtEnd.. well, it's certainly really cool,
-- FIXME: Makefile
-- FIXME: Split index list and other library functions to separate file.
-- FIXME: Match partial column names.
-- FIXME: Use foldrM from Data.Foldable ?
-- FIXME: Import only required functions.
-- FIXME: utf8 support.
-- FIXME: Bytestrings.
-- FIXME: Diabled echo for "check" mode is not convenient. Though, if it is
-- enabled, newline will break all output.

-- Usage: ./show_words mode file [column_names]
--
-- Be aware, that incorrect column names silently skipped without any
-- notification.  This may lead to nasty bugs, when all seems ok, but not
-- works however. So, double check column names in words file and on cmd!
main                =  do
    (mode : file : colNames) <- getArgs
    contents <- readFile file
    hSetEcho stdin False
    putPhrases1 (setMode mode)
        $ reorderPhrases1 colNames (PhrSep '-' " : " " - ")
        $ map (splitToPhrases '-')
        $ lines contents
    putStrLn "Bye!"
  where
    setMode :: String -> (String -> IO String)
    setMode xs
      | xs == "check"   = checkAnswer
      | otherwise       = waitKey

