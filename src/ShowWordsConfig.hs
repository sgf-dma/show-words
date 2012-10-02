
module ShowWordsConfig
    (Config (..)
    )
  where

data Config        = Config
                        { confMode          :: String -> IO String
                        , confInputFile     :: FilePath
                        , confReferenceSep  :: String
                        , confColumnSep     :: String
                        , confPhraseSep     :: String
                        , confColumnNames   :: [String]
                        , confColumnEq      :: String -> String -> Bool
                        , confLineOrder     :: String
                        }

