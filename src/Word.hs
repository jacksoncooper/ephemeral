module Word where

import Prelude hiding (Word)

import Kindle (Export(..), Metadata(..), Annotation(..))

type Author = String
type Title = String
type Location = String
type Text = String

data Word =
  Word Author Title Location Text
  deriving Show

class Words a where
  toWords :: a -> [Word]

instance Words Export where
  toWords (Export (Metadata author title) annotations) =
    map word annotations
    where
      -- TODO: Note the duplicity of the RHS of the following equations.
      -- No good. You probably need to change the Annotation type.
      word (Highlight _ location _ excerpt) =
        Word author title location excerpt
      word (Note location _ excerpt) =
        Word author title location excerpt


