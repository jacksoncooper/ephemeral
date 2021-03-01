module Kindle where

import Data.Char (toLower)
import Data.List (isPrefixOf, stripPrefix)

type Author = String
type Title = String

type Location = String
type Starred = Bool
type Excerpt = String

type Annotation = Either Highlight Note

data Color = Pink | Blue | Yellow | Orange
  deriving Show

data Export =
  Export
    (Maybe Author)
    (Maybe Title)
    [Annotation]
  deriving Show

data Highlight =
  Highlight
    (Maybe Color)
    Location
    (Maybe Starred)
    Excerpt
  deriving Show

data Note =
  Note
    Location
    (Maybe Starred)
    Excerpt
  deriving Show

toAnnotationType :: String -> String
toAnnotationType = map toLower . takeWhile (/= ' ')

toColor :: String -> Maybe Color
toColor text =
  case inParentheses text of
    "Pink"   -> Just Pink
    "Blue"   -> Just Blue
    "Yellow" -> Just Yellow
    "Orange" -> Just Orange
    _        -> Nothing

toStarred :: String -> Maybe Bool
toStarred text =
  case text of
    "Y" -> Just True
    ""  -> Just False
    _   -> Nothing

toAnnotation :: String -> Maybe Annotation
toAnnotation line =
  let attributes = columns line in
  if length attributes == 4 then
    let
      color = toColor $ attributes !! 0
      annotationType = toAnnotationType $ attributes !! 0
      location = attributes !! 1
      starred = toStarred $ attributes !! 2
      excerpt = attributes !! 3
    in case annotationType of
      "highlight" -> Just (Left (Highlight color location starred excerpt))
      "note"      -> Just (Right (Note location starred excerpt))
      _           -> Nothing
  else
    Nothing

toLines :: String -> [String]
toLines =
  let linesOfMetadata = 8 in
  drop linesOfMetadata . lines

readAnnotations :: FilePath -> IO [Maybe Annotation]
readAnnotations highlights =
  (map toAnnotation . toLines) <$> readFile highlights

columns :: String -> [String]
columns = reverse . columns' . reverse 

columns' :: String -> [String]
columns' = go False [] '"'
  where
    go :: Bool -> String -> Char -> String -> [String]
    go _ _ _ [] = []
    go consume column toggle (head : rest) =
      if head == toggle then
        let remaining = go (not consume) [] toggle rest in
        if consume
        then column : remaining
        else remaining
      else
        if consume
        then go True (head : column) toggle rest
        else go False [] toggle rest

between :: Eq a => a -> a -> [a] -> [a]
between start end =
  takeWhile (/= end) . drop 1 . dropWhile (/= start)

inParentheses :: String -> String
inParentheses = between '(' ')'