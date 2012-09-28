
module ShowWordsOptions
    ( Options (..)
    , Config (..)
    )
  where

data Options        = Options
                        { optMode         :: String -> IO String
                        , optLineOrder    :: String
                        , optFile         :: FilePath
                        , optColumnSep    :: String
                        , optPhraseSep    :: String
                        , optReferenceSep :: String
                        }

data Config        = Config
                        { confMode          :: String -> IO String
                        , confInputFile     :: FilePath
                        , confReferenceSep  :: String
                        , confColumnSep     :: String
                        , confPhraseSep     :: String
                        , confColumnNames   :: [String]
                        , confLineOrder     :: String
                        }

