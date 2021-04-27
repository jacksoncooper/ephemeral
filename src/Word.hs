{-# LANGUAGE OverloadedStrings #-}

module Word where

import Prelude hiding (Word)
import Data.Aeson
import Data.Time.Clock (UTCTime)

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
