
import Control.Applicative
import Control.Monad.State

import SgfList

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
strCont :: String -> (String, Bool)
strCont xs          = foldr f ([], False) xs
  where
    f :: Char -> (String, Bool) -> (String, Bool)
    f x ([], s)
      | x == '\\'   = ([], not s)
      | otherwise   = ([x], s)
    f x (zs, s)     = (x : zs, s)

{-
strCont' :: String -> (String, Bool)
strCont' xs       = runState (foldrM (\x -> State . f x) [] xs) False
  where
    f :: Char -> String -> Bool -> (String, Bool)
    f x [] s
      | x == '\\'   = ([], not s)
      | otherwise   = ([x], s)
    f x zs s        = (x : zs, s)-}

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
mergeLines2 xs      = foldr go [[]] xs
  where
    go x []         = let (x', p') = lineCont x
                      in  if p'
                            then zipWith' (++) x' [] : []
                            else [x]
    go x (z : zs)   = let (x', p') = lineCont x
                      in  if p'
                            then zipWith' (++) x' z : zs
                            else x : z : zs
    lineCont :: [String] -> ([String], Bool)
    lineCont        = foldr (\(x, p) (zs, ps) -> (x : zs, p || ps)) ([], False)
                        . map strCont

