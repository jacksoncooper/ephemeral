module Storage
  ( save
  ) where

import Data.Aeson (encodeFile)
import Word as W

save :: [W.Word] -> IO ()
save words =
  encodeFile store words
  where
    store = "highlights.json"
