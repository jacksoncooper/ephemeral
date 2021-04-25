{-# LANGUAGE OverloadedStrings #-}

module Word where

import Prelude hiding (Word)
import Synonyms
import Data.Aeson

data Word =
  Word Author Title Location Excerpt
  deriving Show

instance ToJSON Word where
  toJSON (Word author title location excerpt) =
    object
      [ "author"   .= author
      , "title"    .= title
      , "location" .= location
      , "excerpt"  .= excerpt
      ]
