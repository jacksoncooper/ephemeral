module Kindle where

import Data.Char (toLower)
import Data.List (isPrefixOf, stripPrefix)
import Data.Maybe (catMaybes)

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

type Title = String
type Author = String

data Metadata =
  Metadata Author Title
  deriving Show

data Export =
  Export Metadata [Annotation]
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
  Success f <*> Success a = Success (f a)
  Success _ <*> Error errors = Error errors
  Error errors <*> Success _ = Error errors
  Error errors <*> Error more = Error (errors ++ more)

instance Monad Report where
  Success a >>= f = f a
  Error errors >>= _ = Error errors

-- TODO: Explicitly look for text starting with 'by'.

toAuthor :: String -> Report String
toAuthor  = Success . map toLower . drop 1 . dropWhile (/= ' ')

toTitle :: String -> Report String
toTitle = toLocation

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
    expectCells 4 text >>= \cells ->
      let
        color = toColor (cells !! 0)
        kind = toType (cells !! 0)
        location = toLocation (cells !! 1)
        starred = toStarred (cells !! 2)
        excerpt = pure (cells !! 3)
      in case kind of
        Success Highlight' ->
          Highlight <$> color <*> location <*> starred <*> excerpt
        Success Note' ->
          Note <$> location <*> starred <*> excerpt
        Error errors -> Error errors

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

expectCells :: Int -> String -> Report [String]
expectCells expected text =
  let cells = columns text in
  expectLength "attribute" "attributes" expected cells >> return cells

expectLength :: String -> String -> Int -> [a] -> Report ()
expectLength what whats expected items =
  let
    number = length items
    error =
      "Expected " ++ show expected ++ " "
        ++ (if expected == 1 then what else whats) ++ " but got "
        ++ show number ++ "."
  in
    if number == expected
    then Success ()
    else Error [error]

readMetadata :: FilePath -> IO (Maybe Metadata)
readMetadata path =
  readFile path >>= \text ->
    let
      number = 5
      lines' = take number (lines text)
      report =
        expectLength "line" "lines" number lines' >>
          let
            title = expectCells 1 (lines' !! 1) >>= toTitle . head
            author = expectCells 1 (lines' !! 2) >>= toAuthor . head
          in
            Metadata <$> author <*> title
    in
      case report of
        Success metadata ->
          return (Just metadata)
        Error errors -> 
          putStrLn ("Lines 1 through " ++ show number ++ ":")
            >> printBullets errors
            >> return Nothing

readAnnotation :: Int -> String -> IO (Maybe Annotation)
readAnnotation line text =
  case toAnnotation text of
    Success annotation ->
      return (Just annotation)
    Error errors ->
        putStrLn ("Line " ++ show line ++ ":")
          >> printBullets errors
          >> return Nothing

readAnnotations :: FilePath -> IO [Annotation]
readAnnotations path =
  readFile path >>= \text ->
    let
      lines' = zip [1..] (lines text)
      annotations = traverse (uncurry readAnnotation) lines'
    in
      annotations >>= return . catMaybes

printBullets :: [String] -> IO ()
printBullets items =
  let bullets = map ("  - " ++) items
  in mapM_ putStrLn bullets

excerpt :: Annotation -> String
excerpt (Highlight _ _ _ excerpt) = excerpt
excerpt (Note _ _ excerpt) = excerpt

printAnnotations :: FilePath -> IO ()
printAnnotations path = readAnnotations path >>= mapM_ (putStrLn . excerpt)
