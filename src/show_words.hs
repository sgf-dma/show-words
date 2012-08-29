
module Main
  where

import ShowWords

-- Change corresponding WordSep's field, if you want to change separators.
-- See ShowWords.hs for detailed program usage description.
main :: IO ()
main                = showWords WordsSeps   { columnSep = " : "
                                            , phraseSep = "; "
                                            , referenceSep = " - "
                                            }

