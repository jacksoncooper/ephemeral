module Kindle where

import Data.Char (toLower)
import Data.List (isPrefixOf, stripPrefix)
import Data.Maybe (isJust)

data Color = Pink | Blue | Yellow | Orange
  deriving Show

type Location = String
type Starred = Bool
type Excerpt = String

data Annotation =
    Highlight Color Location Starred Excerpt
  | Note Location Starred Excerpt
  deriving Show

data Type
  = Highlight'
  | Note'
  deriving Show

data Report a =
    Success a
  | Error [String]
  deriving Show

instance Functor Report where
  fmap f (Success a) = Success (f a)
  fmap f (Error errors) = Error errors

instance Applicative Report where
  pure = Success
  (Success f) <*> (Success a) = Success (f a)
  (Success _) <*> (Error errors) = Error errors
  (Error errors) <*> (Success _) = Error errors
  (Error errors) <*> (Error more) = Error (errors ++ more)

toType :: String -> Report Type
toType text =
  let
    kind  = map toLower (takeWhile (/= ' ') text)
    error = "'" ++ kind ++ "' not a recognized annotation type."
  in
    case kind of
      "highlight" -> Success Highlight'
      "note"      -> Success Note'
      _           -> Error [error]

toColor :: String -> Report Color
toColor text =
  let
    color = inParentheses text
    error = "'" ++ color ++ "' not one of Pink, Blue, Yellow, Orange."
  in
    case color of
      "Pink"   -> Success Pink
      "Blue"   -> Success Blue
      "Yellow" -> Success Yellow
      "Orange" -> Success Orange
      _        -> Error [error]

toLocation :: String -> Report String
toLocation = Success . map toLower

toStarred :: String -> Report Bool
toStarred text =
  let
    error = "'" ++ text ++ "' not one of Y or empty."
  in
    case text of
      "Y" -> Success True
      ""  -> Success False
      _   -> Error [error]

toAnnotation :: String -> Report Annotation
toAnnotation text =
  let
    attributes = columns text
    number = length attributes
    expected = 4
    notFour = "Encountered " ++ show number ++ " attributes instead of " ++ show expected ++ "."
  in
    if number == expected then
      let
        color = toColor (attributes !! 0)
        kind = toType (attributes !! 0)
        location = toLocation (attributes !! 1)
        starred = toStarred (attributes !! 2)
        excerpt = pure (attributes !! 3)
      in case kind of
        Success Highlight' ->
          Highlight <$> color <*> location <*> starred <*> excerpt
        Success Note' ->
          Note <$> location <*> starred <*> excerpt
        Error errors -> Error errors
    else
      Error [notFour]

readAnnotations :: FilePath -> IO [Maybe Annotation]
readAnnotations path =
  readFile path >>= \text ->
    let annotations = zip [1..] (lines text)
    in traverse (uncurry readAnnotation) annotations

readAnnotation :: Int -> String -> IO (Maybe Annotation)
readAnnotation line text =
  case toAnnotation text of
    Success annotation ->
      return (Just annotation)
    Error errors ->
      let bullets = map ("  - " ++) errors
      in
        putStrLn ("Line " ++ show line ++ ":")
          >> mapM_ putStrLn bullets
          >> return Nothing

columns :: String -> [String]
columns = map reverse . columns'

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
