module Kindle where

import Data.List (isPrefixOf, stripPrefix)
import Text.Read (readMaybe)

linesOfMetadata = 8

data Color = Pink | Blue | Yellow | Orange
  deriving Show

type Location = Int
type Starred = Bool
type Annotation = String

data Highlight =
  Highlight
    (Maybe Color)
    (Maybe Location)
    (Maybe Starred)
    Annotation
    deriving Show

toColor :: String -> Maybe Color
toColor text =
  case inParentheses text of
    "Pink"   -> Just Pink
    "Blue"   -> Just Blue
    "Yellow" -> Just Yellow
    "Orange" -> Just Orange
    _        -> Nothing

toLocation :: String -> Maybe Int
toLocation location =
  stripPrefix "Location " location >>= readMaybe

toStarred :: String -> Maybe Bool
toStarred text =
  case text of
    "Y" -> Just True
    ""  -> Just False
    _   -> Nothing

isHighlight :: String -> Bool
isHighlight = isPrefixOf "Highlight" . drop 1

toHighlight :: String -> Maybe Highlight
toHighlight line =
  let attributes = columns line in
  if isHighlight line && length attributes == 4
  then Just $
    Highlight
      (toColor $ attributes !! 0)
      (toLocation $ attributes !! 1)
      (toStarred $ attributes !! 2)
      (attributes !! 3)
  else Nothing

readHighlights :: FilePath -> IO [Maybe Highlight]
readHighlights highlights =
  (map toHighlight . toLines) <$> readFile highlights

toLines :: String -> [String]
toLines = drop linesOfMetadata . lines

between :: Eq a => a -> a -> [a] -> [a]
between start end =
  takeWhile (/= end) . drop 1 . dropWhile (/= start)

inParentheses :: String -> String
inParentheses = between '(' ')'

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
