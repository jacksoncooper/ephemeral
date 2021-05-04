module Storage
  ( load
  , save
  ) where

import Control.Exception (IOException, catch)
import Data.Aeson (decodeFileStrict, encodeFile)

import Excerpt

store :: String
store = "highlights.json"

save :: [Excerpt] -> IO ()
save words' =
  doWrite -- `catch` badWrite
  where
    doWrite = encodeFile store words' -- >> return True

-- badWrite :: IOException -> IO Bool
-- badWrite _ = return False

-- TODO: Display the error message in Aeson's parser to the user.
-- Use 'eitherDecodeFileStrict'.

load :: IO (Maybe [Excerpt])
load =
  doRead `catch` badRead
  where
    doRead = decodeFileStrict store

badRead :: IOException -> IO (Maybe [Excerpt])
badRead _ = return (Just [])
