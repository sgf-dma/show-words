
import System.Exit

import Sgf.List

main :: IO ()
main                = do
                        let rs = runAll
                        print rs
                        if all id rs
                          then exitSuccess
                          else exitFailure

runAll :: [Bool]
runAll              = [ runAllTests testIndex
                      , runAllTests testSplitBy
                      , runAllTests testSplitToColumns
                      ]


runAllTests :: (Eq a) => [(a, a)] -> Bool
runAllTests         = all id . runTests

runTests :: (Eq a) => [(a, a)] -> [Bool]
runTests            = foldr (\(x, y) z -> (x == y) : z) []

-- For resolving warnings in testIndex:
--      "Defaulting the following constraint(s) to type `Integer'"
intEq :: Int -> Int -> Bool
intEq               = (==)
-- fst - function call, snd - answer.
--testIndex
testIndex :: [([Int], [Int])]
testIndex           =
    [ (elemByInd 3 [1,2,1]  , [1])
    , (elemByInd 3 [1,2,4]  , [4])
    , (elemByInd 7 [1,2,4]  , [])
    , (elemByInd 3 [1..]    , [3])
    , (elemsByInds []       []      , [])
    , (elemsByInds []       [1..]   , [])
    , (elemsByInds [3..]    []      , [])
    , (elemsByInds [2]      [1..]   , [2])
    , (elemsByInds [1..]    [1]     , [1])
    --, (elemsByInds [2..]    [1]           , [1]) -- Index not found in infinity indexes list.
    , (elemsByInds [2,8,3]  [1,3,2] , [3,2])
    , (elemsByNotInds []        []      , [])
    --, (elemsByNotInds []        [1..]   , [1..]) -- Entire infinity list is result.
    , (elemsByNotInds [3..]     []      , [])
    --, (elemsByNotInds [2..]     [1]     , [1]) -- Index not found in infinity indexes list.
    , (elemsByNotInds [2]       [1]     , [1])
    , (elemsByNotInds [2,8]     [1,2,3] , [1,3])
    , (elemsByNotInds [2,8,1,3] [1,2,3] , [])
    , (indByElem intEq 2 []     , [])
    , (indByElem intEq 2 [1..]  , [2])
    , (indByElem intEq 3 [1,2]  , [])
    , (indsByElems1 intEq []        []       , [])
    , (indsByElems1 intEq []        [1..]    , [])
    , (indsByElems1 intEq [1..]     []       , [])
    , (indsByElems1 intEq [2]       [1..]    , [2])
    , (indsByElems1 intEq [1]       [1, 2, 1], [1])
    , (indsByElems1 intEq [1,1]     [1, 2, 1], [1])
    , (indsByElems1 intEq [1,3,2]   [1, 2, 1], [1,2])
    , (indsByElems1 intEq [1..]     [1, 2, 3], [1, 2, 3])
    --, (indsByElems1 intEq [1..]     [1, 2, 1], [1, 2]) -- Element not found in infinity keys list (because have been deleted).
    --, (indsByElems1 intEq [2..]     [1, 2, 3], [1, 2, 3]) -- Element not found in infinity keys list.
    , (indsByElems intEq []     []      , [])
    , (indsByElems intEq [1..]  []      , [])
    , (indsByElems intEq []     [1..]   , [])
    --, (indsByElems intEq [2,3]        [1..]   , [2,3]) -- Infinity list.
    , (indsByElems intEq [2,3] [3,2,3,2], [1,2,3,4])
    , (indsByElems intEq [2,3] [1,2,1,2], [2,4])
    --, (indsByElems intEq [2..] [1,2]    , [2]) -- Element not found in infinity keys list.
    , (indsByNotElems intEq []          []       , [])
    --, (indsByNotElems intEq []          [1..]    , [1..]) -- Entire infinity list is result.
    , (indsByNotElems intEq [1..]       []       , [])
    , (indsByNotElems intEq [1..]       [1]      , [])
    --, (indsByNotElems intEq [2..]     [1]      , []) -- Element not found in infinity keys list.
    , (indsByNotElems intEq [1]         [1,2,1]  , [2])
    , (indsByNotElems intEq [1,3]       [1,2,1,4], [2,4])
    , (indsByNotElems intEq [1,3,4,2]   [1,2,1,4], [])
    ]

testSplitBy :: [([String], [String])]
testSplitBy         =
    [ (splitBy " - " "a - b - c", ["a", " - ", "b", " - ", "c"])
    , (splitBy " - " "a  -  b - c", ["a ", " - ", " b", " - ", "c"])
    , (splitBy " - " "a  -  b - ", ["a ", " - ", " b", " - ", ""])
    , (splitBy " - " "a  -  b -", ["a ", " - ", " b -"])
    , (splitBy " - " "  -  b -", [" ", " - ", " b -"])
    , (splitBy " - " " -  b -", ["", " - ", " b -"])
    , (splitBy " - " "-  b -", ["-  b -"])
    , (splitBy "- " "-  b -", ["", "- ", " b -"])
    , (splitBy " -" "-  b -", ["-  b", " -", ""])
    , (splitBy "" "-  b -", ["-  b -"])
    , (splitBy " - " " - ", ["", " - ", ""])
    , (splitBy " - " "", [""])
    , (splitBy "" "", [""])
    , (splitBy "-" "a - b - c", ["a ", "-", " b ", "-", " c"])
    , (splitBy " - " " -  - ", ["", " - ", "", " - ", ""])
    , (splitBy "- " "- - ", ["", "- ", "", "- ", ""])
    , (splitBy " -" " - -", ["", " -", "", " -", ""])
    ]

-- This is actually foldrMerge test, because splitToColumns just adds several
-- wraps and unwraps to it.
testSplitToColumns :: [([[String]], [[String]])]
testSplitToColumns =
    [ ( splitToColumns  [" - ", " : "]
                        [ "a - b - c"
                        , "d - e - f"
                        , "g : h : i"
                        , "k : l : m"
                        ]
      , [ ["a", "b", "c"]
        , ["d - e - f"]
        , ["g", "h", "i"]
        , ["k", "l", "m"]
        ]
      )
    , ( splitToColumns  [" - ", " : "]
                        [ "a\\ - b - c"
                        , "d - e - f"
                        , "g : h : i"
                        , "k : l : m\\"
                        ]
      , [ ["ad", "be", "cf"]
        , ["g", "h", "i"]
        , ["k", "l", "m"]
        ]
      )
    , ( splitToColumns  [" - ", " : "]
                        [ "a - b\\ - c"
                        , "d - e - f"
                        , "g : h : i"
                        , "k\\ : l : m"
                        ]
      , [ ["ad", "be", "cf"]
        , ["g", "h", "i"]
        , ["k", "l", "m"]
        ]
      )
    , ( splitToColumns  [" - ", " : "]
                        [ "a - b - \\"
                        , "d - e - f"
                        , "g : h : i"
                        , "k\\ : l\\ : m\\"
                        ]
      , [ ["ad", "be", "f"]
        , ["g", "h", "i"]
        , ["k", "l", "m"]
        ]
      )
    , ( splitToColumns  [" - ", " : "]
                        [ "a\\ - b - c\\"
                        , "d - e - f"
                        , "g : h\\ : i"
                        , "k\\ : l\\ : m\\"
                        ]
      , [ ["ad", "be", "cf"]
        , ["gk", "hl", "im"]
        ]
      )
    , ( splitToColumns  [" - ", " : "]
                        [ "a\\ - b - c\\"
                        , "d - e\\ - f"
                        , "g : h\\ : i"
                        , "k\\ : l\\ : m\\"
                        ]
      , [ ["adg : h\\ : i", "be", "cf"]
        , ["k", "l", "m"]
        ]
      )
    , ( splitToColumns  [" - ", " : "]
                        [ "a\\ - b - c\\"
                        , "d - e\\ - f"
                        , "g : h\\ : i\\"
                        , "k\\ : l\\ : m\\"
                        ]
      , [ ["adg : h\\ : ik\\ : l\\ : m", "be", "cf"]
        ]
      )
    , ( splitToColumns  []
                        [ "a\\ - b - c\\"
                        , "d - e\\ - f"
                        , "g : h\\ : i\\"
                        , "k\\ : l\\ : m\\"
                        ]
      , [ ["a\\ - b - cd - e\\ - f"]
        , ["g : h\\ : ik\\ : l\\ : m"]
        ]
      )
    , ( splitToColumns  [" - ", " : "]
                        []
      , []
      )
    , ( splitToColumns  []
                        []
      , []
      )
    , ( splitToColumns  [" : ", " - "]
                        [ "a\\ - b - c\\"
                        , "d - e\\ - f\\"
                        , "g : h : i"
                        , "k : l : m"
                        ]
      , [ ["a\\ - b - cd - e\\ - fg", "h", "i"]
        , ["k : l : m" ]
        ]
      )
    , ( splitToColumns  [" : ", " - "]
                        [ "a\\ - b - c\\"
                        , "d - e\\ - f\\"
                        , "g\\ : h : i"
                        , "k : l\\ : m"
                        ]
      , [ ["a\\ - b - cd - e\\ - fgk", "hl", "im"]
        ]
      )
    , ( splitToColumns  [" - ", " : "]
                        [ "a\\ - b - c"
                        , "d - e- f"
                        , "g: h : i\\"
                        , "k : l\\ : m"
                        ]
      , [ ["ad", "be- f", "c"]
        , ["g: hk", "il", "m"]
        ]
      )
    , ( splitToColumns  [" - ", " : "]
                        [ "a\\ - b - c"
                        , ""
                        , "\\"
                        , "k : l\\ : m"
                        ]
      , [ ["a", "b", "c"]
        , ["k", "l", "m"]
        ]
      )
    ]

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
hasContinue :: String -> (String, Bool)
hasContinue         = foldr f ([], False)
  where
    f :: Char -> (String, Bool) -> (String, Bool)
    f x ([], s)
      | x == '\\'   = ([], not s)
      | otherwise   = ([x], s)
    f x (zs, s)     = (x : zs, s)

splitToColumns :: [String] -> [String] -> [[String]]
splitToColumns ks   = map getZipList'
                        . fst
                        . flip runBState ks
                        . foldrMerge splitTextLine

-- Folding function for foldrMerge. Split input line to columns using splitBy
-- with current separator (tracked in state). Then filter out all separators
-- and check whether this input line continues on next line. If so, return
-- True, so foldrMerge can mappend it to next input line.
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

