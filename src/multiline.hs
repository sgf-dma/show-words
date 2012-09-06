
import Control.Applicative
import Control.Monad.State

import SgfListIndex

dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd p      = foldr (\x z -> if null z && p x then [] else x : z) []

-- Determine whether string ends at unescaped backslash (escape character is
-- also backslash).
lineCont :: String -> Bool
lineCont            = foldl f False
  where
    f :: Bool -> Char -> Bool
    f s x
      | x == '\\'   = not s
      | otherwise   = False

-- Determine whether string ends at unescaped backslash (escape character is
-- also backslash) and remove trailing backslash sequence.
lineCont2 :: String -> (String, Bool)
lineCont2 xs        = foldr f ([], False) xs
  where
    f :: Char -> (String, Bool) -> (String, Bool)
    f x ([], s)
      | x == '\\'   = ([], not s)
      | otherwise   = ([x], s)
    f x (zs, s)     = (x : zs, s)

{-
lineCont2' :: String -> (String, Bool)
lineCont2' xs       = runState (foldrM (\x -> State . f x) [] xs) False
  where
    f :: Char -> String -> Bool -> (String, Bool)
    f x [] s
      | x == '\\'   = ([], not s)
      | otherwise   = ([x], s)
    f x zs s        = (x : zs, s)-}

-- Add longer tail to the result as is instead of discarding it (like zipWith do).
zipWith' :: (a -> a -> a) -> [a] -> [a] -> [a]
zipWith' _ xs []                = xs
zipWith' _ [] ys                = ys
zipWith' f (x : xs) (y : ys)    = f x y : zipWith' f xs ys

mergeLines :: [[String]] -> [[String]]
mergeLines []       = []
mergeLines xs       = foldr (\x (z : zs) ->
                              if p x
                                then zipWith' (++) (map (dropWhileEnd (== '\\')) x) z : zs
                                else x : z : zs)
                            [[]]
                            xs
  where
    p :: [String] -> Bool
    p               = foldr ((||) . lineCont) False

mergeLines2 :: [[String]] -> [[String]]
mergeLines2 []      = []
mergeLines2 xs      = foldr (\x (z : zs) ->
                             let (x', p') = checkLine x
                             in  if p' then zipWith' (++) x' z : zs
                                   else x : z : zs)
                            [[]]
                            xs
  where
    checkLine :: [String] -> ([String], Bool)
    checkLine       = foldr (\(x, p) (zs, ps) -> (x : zs, p || ps)) ([], False)
                        . map lineCont2

