
module ShowWordsOutput
    ( putStrF
    , waitKey
    , checkAnswer
    , putPhrases
    , showWords3
    )
  where

import System.IO                -- For hSetEcho, hFlush, stdin, stdout.
import Codec.Binary.UTF8.String -- For encode, decode.
import qualified Data.ByteString.Lazy as B
import Data.Monoid
import qualified Data.Foldable as F
import qualified Data.Traversable as T
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
-- FIXME: Pass column separator using ReaderT transformer.
putPhrases :: (String -> IO String) -> WordsSeps -> [Line [String]] -> IO ()
putPhrases f
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
                . concatMap joinPhrases         -- :: -> [IO String]
                . F.foldr (:) []                -- :: -> [[IO String]]
                . inlineSeps (joinColumns sp)   -- :: -> Line [IO String]
                . inlineAction (map (>>= f'))   -- :: -> Line [IO String]
                . fmap (map return)             -- :: -> Line [IO String]
      where
        f' :: String -> IO String
        f' []   = return []
        f' xs   = f xs
        joinColumns :: String -> [IO String] -> [IO String]
        joinColumns sp []           = [return sp]
        joinColumns sp (mx : mxs)   = ((sp ++) <$> mx) : mxs
        joinPhrases :: [IO String] -> [IO String]
        joinPhrases []          = []
        joinPhrases (mx : mxs)  = mx : map ((phrSp ++) <$>) mxs

-- Reimplement joinLine using combinators.
inlineSeps :: T.Traversable t => (a -> a) -> t a -> t a
inlineSeps g        = zipApp (id : repeat g)

putSeps :: T.Traversable t => (IO a -> IO a) -> t a -> t (IO a)
putSeps g           = inlineSeps g . fmap return

inlineAction :: (a -> a) -> Line a -> Line a
inlineAction g      = zipAppOrdered (id : repeat g)

inlineAction' :: (a -> a) -> Line a -> Line a
inlineAction' g     = mappend
                        <$> zipApp (id : repeat g) . onlyOrdered
                        <*> onlyOthers

joinLine' :: (a -> a) -> (a -> a) -> Line a -> [a]
joinLine' f g       = F.foldr (:) [] . inlineSeps g . inlineAction f

joinColumns :: String -> [String] -> [IO String]
joinColumns sp []           = [putStrF sp >> return ""]
joinColumns sp (x : xs)     = (putStrF sp >> return x) : map return xs

joinColumns' :: String -> a -> IO a
joinColumns' sp x           = putStrF sp >> return x

-- v2
putPhrases2 :: (String -> IO String) -> Line [String] -> Line [IO String]
putPhrases2 f       = inlineAction (map (>>= f))
                        -- . fmap (zipApp (id : repeat (putStrF "; ">>)))
                        . fmap (inlineSeps (putStrF "; " >>))
                        . fmap (map return)

putColumns2 :: Line a -> Line (IO a)
putColumns2         = inlineSeps (putStrF " : " >>)
                    -- zipApp (id : repeat (putStrF " : " >>))
                        . fmap return

putLine2 :: (String -> IO String) -> Line [String] -> Line (IO [IO String])
putLine2 f          = putColumns2 . putPhrases2 f

{-
putLine3 f colSp phrSp  = F.foldr (:) [] -- [IO [IO String]]
                            . putSeps (putStrF colSp >>) -- Line (IO [IO String])
                            . inlineAction (map (>>= f)) -- Line [IO String]
                            . fmap (putSeps (putStrF phrSp >>)) -- Line [IO String]-}

-- v3
-- FIXME: Reader?
putLine3 :: (String -> IO String) -> String -> String
            -> Line [String] -> IO (Line ())
putLine3 f colSp phrSp
                    = T.sequence                        -- IO (Line ())
                        . inlineSeps (putStrF colSp >>)
                        . fmap (mapM_ (>>= putStrF))    -- Line (IO ())
                        . inlineAction (map (>>= f))
                        . fmap (inlineSeps (putStrF phrSp >>) . map return) -- Line [IO String]

{-
                    = concatSeq                         -- IO String
                        . inlineSeps (putStrF colSp >>)
                        . fmap concatSeq                -- Line (IO String)
-}

showWords3 :: (String -> IO String) -> WordsSeps -> [Line [String]] -> IO ()
showWords3  f
            (WordsSeps
              { columnSep = colSp
              , phraseSep = phrSp
              , referenceSep = refSp
              })
            (ref : xs)
                    = do
                        putLine3 return refSp "" ref
                        putStrF "\n"
                        mapM_ (\x -> putLine3 f colSp phrSp x >> putStrF "\n") xs

