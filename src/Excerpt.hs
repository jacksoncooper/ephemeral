{-# LANGUAGE OverloadedStrings #-}

module Excerpt where

import Data.Aeson
import Data.Time.Calendar (Day)

data Excerpt = Excerpt
  { author   :: String
  , title    :: String
  , location :: String
  , excerpt  :: String
  , seen     :: (Maybe Day)
  } deriving Show

instance ToJSON Excerpt where
  toJSON (Excerpt author' title' location' excerpt' seen') =
    object
      [ "author"   .= author'
      , "title"    .= title'
      , "location" .= location'
      , "excerpt"  .= excerpt'
      , "seen"     .= seen'
      ]

instance FromJSON Excerpt where
  parseJSON = withObject "Excerpt" $ \value -> Excerpt
    <$> value .: "author"
    <*> value .: "title"
    <*> value .: "location"
    <*> value .: "excerpt"
    <*> value .: "seen"
