module Storage
  ( load
  , save
  ) where

import Control.Exception (IOException, catch)
import Data.Aeson (decodeFileStrict, encodeFile)

import qualified Word as W

store :: String
store = "highlights.json"

save :: [W.Word] -> IO Bool
save words =
  doWrite `catch` badWrite
  where
    doWrite = encodeFile store words >> return True

badWrite :: IOException -> IO Bool
badWrite _ = return False

-- TODO: Display the error message in Aeson's parser to the user.
-- Use 'eitherDecodeFileStrict'.

load :: IO (Maybe [W.Word])
load =
  doRead `catch` badRead
  where
    doRead = decodeFileStrict store

badRead :: IOException -> IO (Maybe [W.Word])
badRead _ = return (Just [])
