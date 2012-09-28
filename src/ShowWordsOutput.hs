
module ShowWordsOutput
    ( putStrF
    , waitKey
    , checkAnswer
    , putLine
    , putPhrases
    )
  where

import System.IO                -- For hSetEcho, hFlush, stdin, stdout.
import Codec.Binary.UTF8.String -- For encode, decode.
import qualified Data.ByteString.Lazy as B
import qualified Data.Foldable as F
import qualified Data.Traversable as T

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


-- FIXME: Tests?
-- FIXME: Reader? Or pass column separator using ReaderT transformer.

-- Inline separators by applying function g to every element, except first.
inlineSeps :: T.Traversable t => (a -> a) -> t a -> t a
inlineSeps g        = zipApp (id : repeat g)

-- Inline action by applying function g to every "ordered" element, except
-- first. First element omitted, because it treated as a question.
inlineAction :: (a -> a) -> Line a -> Line a
inlineAction g      = zipAppOrdered (id : repeat g)

-- Output phrases from a Line and execute specified action before every phrase
-- in all "ordered" columns, except first "ordered" column. First column is
-- omitted, because it is treated as a question.  Other columns (and phrases
-- they contain) will be outputted all at once and execution immediately
-- porceeds to next line.
putLine :: (String -> IO String) -> String -> String -> Line [String] -> IO ()
putLine f colSp phrSp
                    = F.sequence_                       -- IO ()
                        . inlineSeps (putStrF colSp >>)
                        . fmap (mapM_ (>>= putStrF))    -- Line (IO ())
                        . inlineAction (map (>>= f'))
                        . fmap (inlineSeps (putStrF phrSp >>) . map return) -- Line [IO String]
  where
    f' :: String -> IO String
    f' []           = return []
    f' xs           = f xs

-- Output Line-s. First Line is treated as reference (heading) and no action
-- is executed on it and referenceSep is used for joining.
putPhrases :: (String -> IO String) -> WordsSeps -> [Line [String]] -> IO ()
putPhrases _ _ []       = return ()
putPhrases f
         (WordsSeps
            { columnSep = colSp
            , phraseSep = phrSp
            , referenceSep = refSp
            }
         )
         (ref : xs) = do
                        putLine return refSp "" ref
                        putStrF "\n"
                        mapM_ (\x -> putLine f colSp phrSp x >> putStrF "\n")
                              xs

