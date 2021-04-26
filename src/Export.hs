module Export where

data Metadata = Metadata
  { author :: String
  , title :: String
  } deriving Show

data Export a =
  Export Metadata a
  deriving Show
