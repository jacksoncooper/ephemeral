module Storage
  ( save
  ) where

import Data.Aeson (decodeFileStrict, encodeFile)
import qualified Word as W

store :: String
store = "highlights.json"

save :: [W.Word] -> IO ()
save words =
  encodeFile store words

load :: IO (Maybe [W.Word])
load =
  decodeFileStrict store

merge :: [W.Word] -> [W.Word] -> [W.Word]
merge = undefined
