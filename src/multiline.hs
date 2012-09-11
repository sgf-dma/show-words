
import qualified Data.Foldable as F
import Control.Applicative
import SgfList

-- Split each list element to "columns". Each element may be continued on next
-- line. Split function should return whether continue or not. If it is, it'll
-- be merged with next line. Text separators are choosed from list. The last
-- one remains for the remaining text.
splitToCols :: (s -> a -> (b, Bool)) -> (b -> b -> b) -> [s] -> [a] -> [b]
splitToCols split zip ks xs = fst $ runBState (splitToColsM split zip xs) ks

splitToColsM :: (s -> a -> (b, Bool))
             -> (b -> b -> b)
             -> [a]
             -> BState [s] [b]
splitToColsM split zip      = F.foldrM (\x -> BState . f x) []
  where
    -- I can't pattern match against zl (accumulator) in function f, because
    -- this hangs Backward State monad (it'll need to compute first monad
    -- result for computing next state).
    --f :: a -> [b] -> [s] -> ([b], [s])
    f x zl []               = undefined
    f x zl (k : ks)
      | p                   = (x' `goOn` zl, k : ks)
      | null ks             = (x' `add`  zl, [k])
      | otherwise           = (x' `add`  zl, ks)
      where
        (x', p)             = split k x
        goOn _  []          = x' : []
        goOn x' (z : zs)    = zip x' z : zs
        add x' []           = x' : []
        add x' (z : zs)     = x' : z : zs

{-
splitToCols' :: (s -> a -> (b, Bool)) -> (b -> b -> b) -> [s] -> [a] -> [b]
splitToCols' split zip ks xs = fst $ runBState (splitToColsM' split zip xs) ks-}

{-
type Column a       = [a]
-- I have a list of lines ([[a]]) and each line contain elements ([a]).
-- I have a list of separators and each separator is lines as well ([[a]]).
-- I add grouping into each line - columns - [[[a]]]
splitToColsM' :: (a -> a -> Bool)
              -> (Column a -> Bool)
              -> [Column a]
              -> BState [Column a] [[Column a]]
splitToColsM' eq p  = F.foldrM (\x z -> BState $ f x z) []
  where
    --split :: Column a -> Column a -> [Column a]
    split x []          = splitBy eq [] x
    split x (k : ks)    = splitBy eq k  x
    -- I can't pattern match against zl (accumulator) in function f, because
    -- this hangs Backward State monad (it'll need to compute first monad
    -- result for computing next state).
    --f :: a -> [b] -> [s] -> ([[b]], [s])
    f x zl []
      where
        xs'             = splitBy eq k x-}

f :: (F.Foldable t, Eq a) =>
     ([a] -> Bool) -> t a -> [[[a]]] -> [[a]] -> ([[[a]]], [[a]])
f p x zs (k : ks)
  | any p xs'       = (goOn  zs, k : ks)
  | null ks         = (addTo zs, [k])
  | otherwise       = (addTo zs, ks)
  where
    xs'             = splitBy (==) k x
    goOn  []        = xs' : []
    goOn  (z : zs)  = zipWith' (++) xs' z : zs
    addTo []        = xs' : []
    addTo (z : zs)  = xs' : z : zs

{-
    :: [Column a] -> ([[Column a]], [Column a])
    (\xs -> let p' = any p xs
            in  ) . split x-}

f3 :: (F.Foldable t, Alternative f, Eq a)
   => (a -> a -> Bool) -> (f a -> Bool) -> t a -> [[f a]] -> [[a]] -> ([[f a]], [[a]])
f3 eq p x zs []         = (splitBy eq [] x : zs, [])
f3 eq p x zs (k : ks)   =
    let xs' = splitBy eq k x  -- :: -> [f a]
    in  if any p xs' then (xs' `goOn` zs, k : ks)
          else (xs' : zs, ks)
  where
    goOn xs' []         = xs' : []
    goOn xs' (z : zs)   = (zipWith' (<|>) xs' z) : zs

splitToColumns3M :: (F.Foldable t, Alternative f)
                 => (a -> a -> Bool) -> (f a -> Bool)
                 -> t (t a) -> BState [[a]] [[f a]]
splitToColumns3M eq p   = F.foldrM (\x z -> BState (f x z)) []
  where
    --f :: (F.Foldable t, Alternative f, Eq a) =>
    --     t a -> [[f a]] -> [[a]] -> ([[f a]], [[a]])
    f x zs []           = (splitBy eq [] x : zs, [])
    f x zs (k : ks)     =
        let xs' = splitBy eq k x  -- :: -> [f a]
        in  if any p xs' then (xs' `goOn` zs, k : ks)
              else (xs' : zs, ks)
      where
        goOn xs' []         = xs' : []
        goOn xs' (z : zs)   = (zipWith' (<|>) xs' z) : zs

splitToColumns3 :: (F.Foldable t, Alternative f)
                => (a -> a -> Bool) -> (f a -> Bool)
                -> [[a]] -> t (t a) -> [[f a]]
splitToColumns3 eq p ks = fst . flip runBState ks . splitToColumns3M eq p


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

