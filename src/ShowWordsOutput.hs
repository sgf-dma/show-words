
module ShowWordsOutput
    ( putStrF
    , waitKey
    , checkAnswer
    , putPhrases
    , putPhrases'
    )
  where

import System.IO                -- For hSetEcho, hFlush, stdin, stdout.
import Codec.Binary.UTF8.String -- For encode, decode.
import qualified Data.ByteString.Lazy as B
import Data.Monoid
import qualified Data.Foldable as F
import Control.Applicative
import Control.Monad.State

import SgfList
import SgfOrderedLine
import ShowWordsText


putStrF :: String -> IO ()
putStrF x           = do
                        B.putStr . B.pack . encode $ x
                        hFlush stdout

-- Wait for a key from user.
waitKey :: a -> IO a
waitKey p           = getChar >> return p

-- Check that user entered correct phrase.
checkAnswer :: String -> IO String
checkAnswer p       = do
                       r <- getLine
                       return (checkPhrase r ++ p)
  where
    checkPhrase :: String -> String
    checkPhrase []  = ""
    checkPhrase r
      | r == p      = " Ura! "
      | otherwise   = " Ops! "

-- FIXME: Print separators (column or phrase) _before_ answer is asked.
-- Output phrases and execute specified action before every phrase in ordered
-- column, except first ordered column. First is omitted, because it is
-- treated as a question.  Other columns (and phrases they contain) will be
-- outputted all at once and execution immediately porceeds to next line.
putPhrases :: (String -> IO String) -> WordsSeps -> [Line [String]] -> IO ()
putPhrases  f
            (WordsSeps
              { columnSep = colSp
              , phraseSep = phrSp
              , referenceSep = refSp
              })
            (ref : xs)
                    = do
                        putLine refSp ref
                        putStrF "\n"
                        mapM_ (\x -> putLine colSp x >> putStrF "\n") xs
  where
    putLine :: String -> Line [String] -> IO ()
    putLine sp = mapM_ (>>= putStrF)
                . concatMap joinPhrases                    -- :: -> [IO String]
                . joinLine (map (>>= f')) (joinColumns sp) -- :: -> [[IO String]]
                . mapLine1 (map return)                    -- :: -> Line [IO String]
      where
        f' :: String -> IO String
        f' []   = return []
        f' xs   = f xs
        joinColumns :: String -> [IO String] -> [IO String]
        joinColumns sp []           = [return colSp]
        joinColumns sp (mx : mxs)   = ((sp ++) <$> mx) : mxs
        joinPhrases :: [IO String] -> [IO String]
        joinPhrases []          = []
        joinPhrases (mx : mxs)  = mx : map ((phrSp ++) <$>) mxs

-- Rewrite using new interface.
putPhrases' :: (String -> IO String) -> WordsSeps -> [Line [String]] -> IO ()
putPhrases' f
            (WordsSeps
              { columnSep = colSp
              , phraseSep = phrSp
              , referenceSep = refSp
              })
            (ref : xs)
                    = do
                        putLine refSp ref
                        putStrF "\n"
                        mapM_ (\x -> putLine colSp x >> putStrF "\n") xs
  where
    putLine :: String -> Line [String] -> IO ()
    putLine sp = mapM_ (>>= putStrF)
                . concatMap joinPhrases                    -- :: -> [IO String]
                . F.foldr (:) []
                . inlineSeps (joinColumns sp)
                . inlineAction (map (>>= f'))
                . mapLine1 (map return)                    -- :: -> Line [IO String]
      where
        f' :: String -> IO String
        f' []   = return []
        f' xs   = f xs
        joinColumns :: String -> [IO String] -> [IO String]
        joinColumns sp []           = [return colSp]
        joinColumns sp (mx : mxs)   = ((sp ++) <$> mx) : mxs
        joinPhrases :: [IO String] -> [IO String]
        joinPhrases []          = []
        joinPhrases (mx : mxs)  = mx : map ((phrSp ++) <$>) mxs

-- Reimplement joinLine using combinators.
inlineSeps :: (a -> a) -> Line a -> Line a
inlineSeps g        = zipApp (id : repeat g)

{-
mapAction :: (a -> a) -> Line a -> Line a
mapAction g         = fst . flip runState (id : repeat g) . mapOrderedM f
  where
    f x             = do
                        (f : fs) <- get
                        put fs
                        return (f x)-}

inlineAction :: (a -> a) -> Line a -> Line a
inlineAction g      = zipAppOrdered (id : repeat g)

inlineAction' :: (a -> a) -> Line a -> Line a
inlineAction' g     = mappend
                        <$> zipApp (id : repeat g) . onlyOrdered
                        <*> onlyOthers

inlineAction'' :: (a -> a) -> Line a -> Line a
inlineAction'' g x   = let xOrd = onlyOrdered x
                           xOth = onlyOthers x
                       in  (zipApp (id : repeat g) xOrd) `mappend` xOth

joinLine' :: (a -> a) -> (a -> a) -> Line a -> [a]
joinLine' f g       = F.foldr (:) [] . inlineSeps g . inlineAction f

