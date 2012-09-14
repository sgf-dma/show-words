
import Control.Applicative

import SgfOrderedLine
import ShowWordsText

-- NOTE: In order to compile this file, SgfOrderedLine file must be edit to
-- import Line data constructor.

runAll :: [Bool]
runAll              = runAllTests testSplitText
                        : map (runAllTests . testShowWords) [showWords1, showWords2]

runAllTests :: (Eq a) => [(a, a)] -> Bool
runAllTests         = all id . runTests

runTests :: (Eq a) => [(a, a)] -> [Bool]
runTests            = foldr (\(x, y) z -> (x == y) : z) []

wsp = WordsSeps   {columnSep = " : ", phraseSep = "; ", referenceSep = " - "}

-- This funcion just specifies the order in which splitToColumns and
-- splitToPhrases should be called.
splitText :: WordsSeps -> [String] -> [[[String]]]
splitText           = (.) <$> splitToPhrases <*> splitToColumns

testSplitText :: [ ([[[String]]], [[[String]]]) ]
testSplitText   = 
    [ ( splitText wsp   [ "a - b - c"
                        , "d - e - f"
                        , "g : h : i"
                        , "k : l : m"
                        ]
      , [ [["a"], ["b"], ["c"]]
        , [["d - e - f"]]
        , [["g"], ["h"], ["i"]]
        , [["k"], ["l"], ["m"]]
        ]
      )
    , ( splitText wsp   [ "a; A - b - c"
                        , "d - e - f; "
                        , "; g : h;  : i"
                        , "k : l; L : m"
                        ]
      , [ [["a; A"], ["b"], ["c"]]
        , [["d - e - f", ""]]
        , [["", "g"], ["h", ""], ["i"]]
        , [["k"], ["l", "L"], ["m"]]
        ]
      )
    , ( splitText wsp   [ "a; A - b\\;  - c"
                        , "d - e - f; "
                        , "; g : h;  : i"
                        , "k : l; L : m"
                        ]
      , [ [["a; A"], ["b\\; "], ["c"]]
        , [["d - e - f", ""]]
        , [["", "g"], ["h", ""], ["i"]]
        , [["k"], ["l", "L"], ["m"]]
        ]
      )
    , ( splitText wsp   [ "a; A - b\\;  - \\; c"
                        , "d - e - f; "
                        , "; g : h;  : i"
                        , "k : l; L : m"
                        ]
      , [ [["a; A"], ["b\\; "], ["\\; c"]]
        , [["d - e - f", ""]]
        , [["", "g"], ["h", ""], ["i"]]
        , [["k"], ["l", "L"], ["m"]]
        ]
      )
    , ( splitText wsp   [ "a; A; \\ - ; b - c"
                        , "d - e - f; "
                        , "; g;  : h;  : i"
                        , "k : ; l; L : m"
                        ]
      , [ [["a; A; d"], ["; be"], ["cf; "]]
        , [["", "g", ""], ["h", ""], ["i"]]
        , [["k"], ["", "l", "L"], ["m"]]
        ]
      )
    , ( splitText wsp   [ "a; A - ; b\\;  - \\; c\\"
                        , "\\d - e - \\f; "
                        , "; g : h;  : i"
                        , "k : \\; l; L : m"
                        ]
      , [ [["a; A\\d"], ["; b\\; e"], ["\\; c\\f; "]]
        , [["", "g"], ["h", ""], ["i"]]
        , [["k"], ["\\", "l", "L"], ["m"]]
        ]
      )
    , ( splitText wsp   [ "a; A - ; b\\;  - \\; c\\"
                        , "\\d - e - \\f; "
                        , "; g : h; \\ : ; "
                        , " : \\; l;L : m"
                        ]
      , [ [["a; A\\d"], ["; b\\; e"], ["\\; c\\f; "]]
        , [["", "g"], ["h", "\\", "l;L"], ["", "m"]]
        ]
      )
    , ( splitText wsp   [ ";  - \\;  - \\\\\\"
                        , "\\; - ;  - "
                        , "; : ; \\ : ;"
                        , "\\;  : \\; : \\"
                        ]
      , [ [["; \\;"], ["\\; ; "], [""]]
        , [[";\\", ""], ["", "\\;"], [";"]]
        ]
      )
    , ( splitText wsp   [ ";  - \\;  - \\\\\\"
                        , "\\; - ;  - \\"
                        , "; : ;: ;"
                        , "\\;  : \\; : \\"
                        ]
      , [ [["; \\;; : ;: ;"], ["\\; ; "], [""]]
        , [["\\", ""], ["\\;"], [""]]
        ]
      )
    , ( splitText wsp   [ "a; A - ; b\\;  - \\; c\\"
                        , "\\d- e - \\f; "
                        , "; g : h; \\ : ; "
                        , ": \\; l;L : m"
                        ]
      , [ [["a; A\\d- e"], ["; b\\; \\f; "], ["\\; c"]]
        , [["", "g: \\", "l;L"], ["h", "m"], ["", ""]]
        ]
       )
     , ( splitText wsp  [ " - -\\"
                        , "\\d- e - \\f; "
                        , "; g: h; \\ : ; "
                        , "k : \\; l : "
                        ]
     , [ [["\\d- e"], ["-\\f; "]]
       , [["", "g: h", "k"], ["", "\\", "l"], [""]]
       ]
     )
     , ( splitText wsp  [ " - -\\"
                        , " - ; "
                        , "; ; \\ : "
                        , " : ;  : "
                        ]
     , [ [[""], ["-; "]]
       , [["", "", ""], ["", ""], [""]]
       ]
     )
     , ( splitText wsp  [ " - -\\"
                        , " - ; "
                        ]
     , [ [[""], ["-; "]]
       ]
     )
     , ( splitText wsp  [ " - \\"
                        ]
     , [ [[""], [""]]
       ]
     )
     , ( splitText wsp  []
     , [ ]
     )
    , ( splitText (WordsSeps
                    { columnSep = ""
                    , phraseSep = ""
                    , referenceSep = ""
                    })  [ "a; A - ; b\\;  - \\; c\\"
                        , "\\d - e - \\f; "
                        , "; g : h; \\ : i"
                        , "k : \\; l; L : m"
                        ]
      , [ [["a; A - ; b\\;  - \\; c\\d - e - \\f; "]]
        , [["; g : h; \\ : i"]]
        , [["k : \\; l; L : m"]]
        ]
      )
    , ( splitText (WordsSeps
                    { columnSep = ""
                    , phraseSep = ""
                    , referenceSep = ""
                    })  [ "a; A - ; b\\;  - \\; c\\"
                        , "\\d - e - \\f; "
                        , "; g : h; \\ : i\\"
                        , "k : \\; l; L : m"
                        ]
      , [ [["a; A - ; b\\;  - \\; c\\d - e - \\f; "]]
        , [["; g : h; \\ : ik : \\; l; L : m"]]
        ]
      )
    ]

-- This is not real showWods, just part of it. Here are two possible sequences
-- of ShowWordsText functions, which may be used in showWords.
showWords1 :: [String] -> [String] -> [Line [String]]
showWords1  colNames    = splitToPhrases wsp
                            . reorderColumns (==) colNames
                            . splitToColumns wsp

showWords2 :: [String] -> [String] -> [Line [String]]
showWords2  colNames    = reorderColumns refEq (map (: []) colNames)
                            . splitToPhrases wsp
                            . splitToColumns wsp
  where
    refEq :: [String] -> [String] -> Bool
    refEq xs ys             = (normalize xs) == (normalize ys)
      where
        normalize           = concat . map dropSpaces

testShowWords :: ([String] -> [String] -> [Line [String]])
              -> [ ([Line [String]], [Line [String]]) ]
testShowWords showWords =
    [ ( showWords ["a", "b", "c"]   [ "a - b - c"
                                    , "d - e - f"
                                    , "g : h : i"
                                    , "k : l : m : n"
                                    ]
      , [ Line [] [["a"], ["b"], ["c"]]
        , Line [["d - e - f"]] []
        , Line [["g"], ["h"], ["i"]] []
        , Line [["k"], ["l"], ["m"]] [["n"]]
        ]
      )
    , ( showWords ["c", "a", "d", "b"]  [ "a - b - c"
                                        , "d - e - f"
                                        , "g : h : i"
                                        , "k : l : m : n"
                                        ]
      , [ Line [] [["c"], ["a"], ["b"]]
        , Line [["d - e - f"]] []
        , Line [["i"], ["g"], ["h"]] []
        , Line [["m"], ["k"], ["l"]] [["n"]]
        ]
      )
    , ( showWords ["", "c", "a"]    [ "a - b - c"
                                    , "d - e - f"
                                    , "g : h : i"
                                    , "k : l : m : n"
                                    ]
      , [ Line [] [["c"], ["a"], ["b"]]
        , Line [["d - e - f"]] []
        , Line [["i"], ["g"]] [["h"]]
        , Line [["m"], ["k"]] [["l"], ["n"]]
        ]
      )
    , ( showWords ["f", "b -ce", "ed"]   [ "a\\ - b -c"
                                    , "d - e - f"
                                    , "g : h : i"
                                    , "k : l : m : n"
                                    ]
      , [ Line [] [["f"], ["b -ce"], ["ad"]]
        , Line [["i"], ["h"]] [["g"]]
        , Line [["m"], ["l"]] [["k"], ["n"]]
        ]
      )
    , ( showWords ["a"]             [ "a - b - c"
                                    , "d - e - f"
                                    , "g : h : i"
                                    , "k : l : m : n"
                                    ]
      , [ Line [] [["a"], ["b"], ["c"]]
        , Line [["d - e - f"]] []
        , Line [["g"]] [["h"], ["i"]]
        , Line [["k"]] [["l"], ["m"], ["n"]]
        ]
      )
    , ( showWords ["bf"]            [ "a - b\\ - c"
                                    , "d -e - f"
                                    , "g : h : i"
                                    , "k : l : m : n"
                                    ]
      , [ Line [] [["bf"], ["ad -e"], ["c"]]
        , Line [["h"]] [["g"], ["i"]]
        , Line [["l"]] [["k"], ["m"], ["n"]]
        ]
      )
    , ( showWords []                [ "a - b - c"
                                    , "d - e - f"
                                    , "g : h : i"
                                    , "k : l : m : n"
                                    ]
      , [ Line [] [["a"], ["b"], ["c"]]
        , Line [] [["d - e - f"]]
        , Line [] [["g"], ["h"], ["i"]]
        , Line [] [["k"], ["l"], ["m"], ["n"]]
        ]
      )
    , ( showWords ["a", "cf", "a; d"]   [ "a;  - b - c\\"
                                        , "d -  e - f"
                                        , "g : h : i"
                                        , "k : l : m : n"
                                        ]
      , [ Line [] [["cf"], ["a; d"], ["b e"]]
        , Line [["i"], ["g"]] [["h"]]
        , Line [["m"], ["k"]] [["l"], ["n"]]
        ]
      )
    , ( showWords ["a", "cf", "a; d"]   [ "a;  - b - c\\"
                                        , "d -  e - f"
                                        ]
      , [ Line [] [["cf"], ["a; d"], ["b e"]]
        ]
      )
    , ( showWords ["a", "c", "a; "]     [ "a;  - b - c\\"
                                        ]
      , [ Line [] [["c"], ["a; "], ["b"]]
        ]
      )
    , ( showWords ["a", "c", "a; "]     [ "a;  : b : c\\"
                                        ]
      , [ Line [] [["a;  : b : c"]]
        ]
      )
    , ( showWords ["a", "c", "a; "]   []
      , []
      )
    , ( showWords []   []
      , []
      )
    ]

test1 = showWords1 ["bf"]            [ ]

