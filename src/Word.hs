module Word where

import Prelude hiding (Word)

import Synonyms

data Word =
  Word Author Title Location Excerpt
  deriving Show
