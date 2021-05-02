{-# LANGUAGE OverloadedStrings #-}

module Word where

import Prelude hiding (Word)

import Data.Aeson
import Data.Time.Clock (UTCTime(..), getCurrentTime)

data Word = Word
  { author   :: String
  , title    :: String
  , location :: String
  , excerpt  :: String
  , seen     :: (Maybe UTCTime)
  } deriving Show

instance ToJSON Word where
  toJSON (Word author title location excerpt seen) =
    object
      [ "author"   .= author
      , "title"    .= title
      , "location" .= location
      , "excerpt"  .= excerpt
      , "seen"     .= seen
      ]

instance FromJSON Word where
  parseJSON = withObject "Word" $ \value -> Word
    <$> value .: "author"
    <*> value .: "title"
    <*> value .: "location"
    <*> value .: "excerpt"
    <*> value .: "seen"

see :: Word -> IO Word
see word = do
  (UTCTime day _) <- getCurrentTime
  let time = UTCTime day 0
  return (word { seen = Just time })
