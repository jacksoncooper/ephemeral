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
  -- This is very bad.
  let attributes = split ',' line in
  if isHighlight line && length attributes == 4
  then Just $
    Highlight
      (toColor . inQuotes $ attributes !! 0)
      (toLocation . inQuotes $ attributes !! 1)
      (toStarred . inQuotes $ attributes !! 2)
      (inQuotes $ attributes !! 3)
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

inQuotes :: String -> String
inQuotes = between '"' '"'

split :: Char -> String -> [String]
split character text =
  -- TODO: This destroys highlights that have commas in them because Amazon
  -- doesn't sanitize the annotations export.
  let (head, rest) = drop 1 <$> break (== ',') text in
  if head == "" then []
  else head : split character rest