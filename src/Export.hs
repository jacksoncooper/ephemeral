module Export where

import Synonyms

data Metadata =
  Metadata Author Title
  deriving Show

data Export a =
  Export Metadata a
  deriving Show
