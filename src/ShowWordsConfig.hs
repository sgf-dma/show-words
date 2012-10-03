
module ShowWordsConfig
    ( Config (..)
    , defaultConf
    )
  where

data Config         =
    Config
      { confAction              :: String -> IO String
      , confInputFile           :: FilePath
      , confReferenceSep        :: String
      , confColumnSep           :: String
      , confPhraseSep           :: String
      , confColumnNames         :: [String]
      , confColumnEq            :: String -> String -> Bool
      , confLineOrder           :: String
      , confOutputMode          :: String
      , confOutputReferenceSep  :: String
      , confOutputColumnSep     :: String
      , confOutputPhraseSep     :: String
      }

defaultConf :: Config
defaultConf         =
    Config
      { confAction              = return
      , confInputFile           = "./words.txt"
      , confReferenceSep        = " - "
      , confColumnSep           = " : "
      , confPhraseSep           = "; "
      , confColumnEq            = (==)
      , confColumnNames         = []
      , confLineOrder           = "disabled"
      -- Note, that cmd separator options change both output and (input)
      -- separators (confReferenceSep, ..) to the same new value, so it's not
      -- recommended to set them to different defaults. Now output separators
      -- used only to implement output modes.
      , confOutputMode          = "line"
      , confOutputReferenceSep  = confReferenceSep defaultConf
      , confOutputColumnSep     = confColumnSep defaultConf
      , confOutputPhraseSep     = confPhraseSep defaultConf
      }

