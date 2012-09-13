
import SgfList

runAllTests :: (Eq a) => [(a, a)] -> Bool
runAllTests         = all id . runTests

runTests :: (Eq a) => [(a, a)] -> [Bool]
runTests            = foldr (\(x, y) z -> (x == y) : z) []

-- fst - function call, snd - answer.
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
                        [ "a - b - c\\"
                        , "d - e - f"
                        , "g : h : i"
                        , "k\\ : l\\ : m\\"
                        ]
      , [ ["ad", "be", "cf"]
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
    ]

strCont :: String -> (String, Bool)
strCont xs          = foldr f ([], False) xs
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
    --split :: String -> String -> ([String], Bool)
    split k         = foldr (\(x, p) (zx, zp) -> (x : zx, p || zp)) ([], False)
                        . map strCont
                        . filter (/= k)
                        . splitBy k
    --f :: String -> [String] -> (([String], Bool), [String])
    f x []          = (split [] x, [])
    f x (k : ks)
      | null ks     = (z, [k])
      | otherwise   = if p
                        then (z, k : ks)
                        else (z, ks)
      where
        z@(_, p)    = split k x

