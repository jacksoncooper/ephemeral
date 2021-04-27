module Storage
  ( load
  , save
  ) where

import Data.Aeson (decodeFileStrict, encodeFile)
import qualified Word as W

store :: String
store = "highlights.json"

save :: [W.Word] -> IO ()
save words =
  encodeFile store words

-- TODO:
--   - 'decodeFileStrict' throws an exception when the file does not exist.
--   - Display the error message in Aeson's parser to the user.
--     Use 'eitherDecodeFileStrict'.

load :: IO (Maybe [W.Word])
load =
  decodeFileStrict store
