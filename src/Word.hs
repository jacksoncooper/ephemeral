module Word where

import Prelude hiding (Word)

import Kindle (Annotation, excerpt)

data Word =
  Word String
  deriving Show

class Wordable a where
  word :: a -> Word

instance Wordable Annotation where
  word = Word . excerpt
