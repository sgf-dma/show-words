
import Data.Function (on)
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Reader

import Sgf.OrderedLine
import ShowWords.Config (Config (..))
import ShowWords.Text

main :: IO ()
main                = print runAll

runAll :: [Bool]
runAll              = runAllTests testSplitText
                        : map (runAllTests . testShowWords) [showWords1, showWords2]

runAllTests :: (Eq a) => [(a, a)] -> Bool
runAllTests         = all id . runTests

runTests :: (Eq a) => [(a, a)] -> [Bool]
runTests            = foldr (\(x, y) z -> (x == y) : z) []

testConf :: Config
testConf            = Config
                        { confReferenceSep  = " - "
                        , confColumnSep     = " : "
                        , confPhraseSep     = "; "
                        , confColumnEq      = (==)
                        }

-- This funcion just specifies the order in which splitToColumns and
-- splitToPhrases should be called.
splitText :: (Config -> Config) -> [String] -> [[[String]]]
splitText f         = runIdentity 
                        . flip runReaderT testConf
                        . local f
                        . (splitToPhrases <=< splitToColumns)

testSplitText :: [ ([[[String]]], [[[String]]]) ]
testSplitText   = 
    [ ( splitText id    [ "a - b - c"
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
    , ( splitText id    [ "a; A - b - c"
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
    , ( splitText id    [ "a; A - b\\;  - c"
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
    , ( splitText id    [ "a; A - b\\;  - \\; c"
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
    , ( splitText id    [ "a; A; \\ - ; b - c"
                        , "d - e - f; "
                        , "; g;  : h;  : i"
                        , "k : ; l; L : m"
                        ]
      , [ [["a; A; d"], ["; be"], ["cf; "]]
        , [["", "g", ""], ["h", ""], ["i"]]
        , [["k"], ["", "l", "L"], ["m"]]
        ]
      )
    , ( splitText id    [ "a; A - ; b\\;  - \\; c\\"
                        , "\\d - e - \\f; "
                        , "; g : h;  : i"
                        , "k : \\; l; L : m"
                        ]
      , [ [["a; A\\d"], ["; b\\; e"], ["\\; c\\f; "]]
        , [["", "g"], ["h", ""], ["i"]]
        , [["k"], ["\\", "l", "L"], ["m"]]
        ]
      )
    , ( splitText id    [ "a; A - ; b\\;  - \\; c\\"
                        , "\\d - e - \\f; "
                        , "; g : h; \\ : ; "
                        , " : \\; l;L : m"
                        ]
      , [ [["a; A\\d"], ["; b\\; e"], ["\\; c\\f; "]]
        , [["", "g"], ["h", "\\", "l;L"], ["", "m"]]
        ]
      )
    , ( splitText id    [ ";  - \\;  - \\\\\\"
                        , "\\; - ;  - "
                        , "; : ; \\ : ;"
                        , "\\;  : \\; : \\"
                        ]
      , [ [["; \\;"], ["\\; ; "], [""]]
        , [[";\\", ""], ["", "\\;"], [";"]]
        ]
      )
    , ( splitText id    [ ";  - \\;  - \\\\\\"
                        , "\\; - ;  - \\"
                        , "; : ;: ;"
                        , "\\;  : \\; : \\"
                        ]
      , [ [["; \\;; : ;: ;"], ["\\; ; "], [""]]
        , [["\\", ""], ["\\;"], [""]]
        ]
      )
    , ( splitText id    [ "a; A - ; b\\;  - \\; c\\"
                        , "\\d- e - \\f; "
                        , "; g : h; \\ : ; "
                        , ": \\; l;L : m"
                        ]
      , [ [["a; A\\d- e"], ["; b\\; \\f; "], ["\\; c"]]
        , [["", "g: \\", "l;L"], ["h", "m"], ["", ""]]
        ]
       )
     , ( splitText id   [ " - -\\"
                        , "\\d- e - \\f; "
                        , "; g: h; \\ : ; "
                        , "k : \\; l : "
                        ]
     , [ [["\\d- e"], ["-\\f; "]]
       , [["", "g: h", "k"], ["", "\\", "l"], [""]]
       ]
     )
     , ( splitText id   [ " - -\\"
                        , " - ; "
                        , "; ; \\ : "
                        , " : ;  : "
                        ]
     , [ [[""], ["-; "]]
       , [["", "", ""], ["", ""], [""]]
       ]
     )
     , ( splitText id   [ " - -\\"
                        , " - ; "
                        ]
     , [ [[""], ["-; "]]
       ]
     )
     , ( splitText id   [ " - \\"
                        ]
     , [ [[""], [""]]
       ]
     )
     , ( splitText id   []
     , [ ]
     )
    , ( splitText   (\conf -> conf
                                { confReferenceSep = ""
                                , confColumnSep = ""
                                , confPhraseSep = ""
                                }
                    )
                    [ "a; A - ; b\\;  - \\; c\\"
                    , "\\d - e - \\f; "
                    , "; g : h; \\ : i"
                    , "k : \\; l; L : m"
                    ]
      , [ [["a; A - ; b\\;  - \\; c\\d - e - \\f; "]]
        , [["; g : h; \\ : i"]]
        , [["k : \\; l; L : m"]]
        ]
      )
    , ( splitText   (\conf -> conf
                                { confReferenceSep = ""
                                , confColumnSep = ""
                                , confPhraseSep = ""
                                }
                    )
                    [ "a; A - ; b\\;  - \\; c\\"
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
showWords1 :: (Config -> Config) -> [String] -> [Line [String]]
showWords1 f contents   = runIdentity
                            . flip runReaderT testConf
                            . local f
                            $ do
                                xs <- splitToColumns contents
                                Config {confColumnNames = colNames} <- ask
                                splitToPhrases
                                    . reorderColumns (==) colNames
                                    $ xs

showWords2 :: (Config -> Config) -> [String] -> [Line [String]]
showWords2 f contents   = runIdentity
                            . flip runReaderT testConf
                            . local f
                            $ do
                                xs <- splitToPhrases
                                        <=< splitToColumns
                                        $ contents
                                colNames <- getColNames
                                colEq <- getColEq
                                return (reorderColumns colEq colNames xs)
  where
    getColNames :: (Monad m) => ReaderT Config m [[String]]
    getColNames     = ask >>= return . map (: []) . confColumnNames
    getColEq :: (Monad m) => ReaderT Config m ([String] -> [String] -> Bool)
    getColEq        = do
        Config {confColumnEq = eq} <- ask
        return (eq `on` normalize)
      where
        normalize   = concatMap dropSpaces

testShowWords :: ((Config -> Config) -> [String] -> [Line [String]])
              -> [ ([Line [String]], [Line [String]]) ]
testShowWords showWords =
    [ ( showWords   (\conf -> conf {confColumnNames = ["a", "b", "c"]})
                    [ "a - b - c"
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
    , ( showWords   (\conf -> conf {confColumnNames = ["c", "a", "d", "b"]})
                    [ "a - b - c"
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
    , ( showWords   (\conf -> conf {confColumnNames = ["", "c", "a"]})
                    [ "a - b - c"
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
    , ( showWords   (\conf -> conf {confColumnNames = ["f", "b -ce", "ed"]})
                    [ "a\\ - b -c"
                    , "d - e - f"
                    , "g : h : i"
                    , "k : l : m : n"
                    ]
      , [ Line [] [["f"], ["b -ce"], ["ad"]]
        , Line [["i"], ["h"]] [["g"]]
        , Line [["m"], ["l"]] [["k"], ["n"]]
        ]
      )
    , ( showWords   (\conf -> conf {confColumnNames = ["a"]})
                    [ "a - b - c"
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
    , ( showWords   (\conf -> conf {confColumnNames = ["bf"]})
                    [ "a - b\\ - c"
                    , "d -e - f"
                    , "g : h : i"
                    , "k : l : m : n"
                    ]
      , [ Line [] [["bf"], ["ad -e"], ["c"]]
        , Line [["h"]] [["g"], ["i"]]
        , Line [["l"]] [["k"], ["m"], ["n"]]
        ]
      )
    , ( showWords   (\conf -> conf {confColumnNames = []})
                    [ "a - b - c"
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
    , ( showWords   (\conf -> conf {confColumnNames = ["a", "cf", "a; d"]})
                    [ "a;  - b - c\\"
                    , "d -  e - f"
                    , "g : h : i"
                    , "k : l : m : n"
                    ]
      , [ Line [] [["cf"], ["a; d"], ["b e"]]
        , Line [["i"], ["g"]] [["h"]]
        , Line [["m"], ["k"]] [["l"], ["n"]]
        ]
      )
    , ( showWords   (\conf -> conf {confColumnNames = ["a", "cf", "a; d"]})
                    [ "a;  - b - c\\"
                    , "d -  e - f"
                    ]
      , [ Line [] [["cf"], ["a; d"], ["b e"]]
        ]
      )
    , ( showWords   (\conf -> conf {confColumnNames = ["a", "c", "a; "]})
                    [ "a;  - b - c\\"
                    ]
      , [ Line [] [["c"], ["a; "], ["b"]]
        ]
      )
    , ( showWords   (\conf -> conf {confColumnNames = ["a", "c", "a; "]})
                    [ "a;  : b : c\\"
                    ]
      , [ Line [] [["a;  : b : c"]]
        ]
      )
    , ( showWords   (\conf -> conf {confColumnNames = ["a", "c", "a; "]})
                    []
      , []
      )
    , ( showWords   (\conf -> conf {confColumnNames = []})
                    []
      , []
      )
    ]

