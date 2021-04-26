{-# LANGUAGE OverloadedStrings #-}

module Word where

import Prelude hiding (Word)
import Data.Aeson

data Word = Word
  { author :: String
  , title :: String
  , location :: String
  , excerpt :: String
  } deriving Show

instance ToJSON Word where
  toJSON (Word author title location excerpt) =
    object
      [ "author"   .= author
      , "title"    .= title
      , "location" .= location
      , "excerpt"  .= excerpt
      ]

instance FromJSON Word where
  parseJSON = withObject "Word" $ \value -> Word
    <$> value .: "author"
    <*> value .: "title"
    <*> value .: "location"
    <*> value .: "excerpt"
