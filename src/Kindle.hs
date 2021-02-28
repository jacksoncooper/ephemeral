module Kindle where

import Data.List (stripPrefix)
import Text.Read (readMaybe)

linesOfMetadata = 7

data Color = Pink | Blue | Yellow | Orange
  deriving Show

type Location = Int
type Starred = Bool
type Annotation = String

data Highlight = Highlight Color Location Starred Annotation
  deriving Show

between :: Eq a => a -> a -> [a] -> [a]
between start end =
  takeWhile (/= end) . drop 1 . dropWhile (/= start)

toColor :: String -> Maybe Color
toColor text =
  let color = between '(' ')' text
  in case color of
    "Pink"   -> Just Pink
    "Blue"   -> Just Blue
    "Yellow" -> Just Yellow
    "Orange" -> Just Orange
    _        -> Nothing

toLocation :: String -> Maybe Int
toLocation location =
  stripPrefix "Location " location >>= readMaybe

toStarred :: String -> Bool
toStarred = (== "Y")

toHighlight :: String -> Highlight
toHighlight line = undefined

toLines :: String -> [String]
toLines = drop linesOfMetadata . lines

readHighlights :: FilePath -> IO [Highlight]
readHighlights highlights =
  (map toHighlight) <$> toLines <$> readFile highlights
