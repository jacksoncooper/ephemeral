module Kindle where

import Data.Char (toLower)
import Data.Maybe (catMaybes)

-- TODO:
-- 1. Write parse errors to file instead of standard output.
-- 2. Explicitly look for text starting with 'by' when parsing author, error
--    with something like
--      Expected 'by' to come before author name but found 'bleh'.
-- 3. Parse errors in author and title are ambiguous and don't include a line
--    number.

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

data Export = Export
  { exportMetadata :: Metadata
  , exportAnnotations :: [Annotation]
  }
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

excerpt :: Annotation -> String
excerpt (Highlight _ _ _ excerpt) = excerpt
excerpt (Note _ _ excerpt) = excerpt

labelAndItemize :: String -> Report a -> Report a
labelAndItemize more (Error errors) = Error (more : map ("  - " ++ ) errors)
labelAndItemize _ report = report

elide :: String -> String
elide text =
  let words' = words text
  in
    if length words' > 10
    then (unwords . take 10) words' ++ "…"
    else text

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
    color = (map toLower . inParentheses) text
    error = "'" ++ color ++ "' not one of 'pink', 'blue', 'yellow', 'orange'."
  in
    case color of
      "pink"   -> Success Pink
      "blue"   -> Success Blue
      "yellow" -> Success Yellow
      "orange" -> Success Orange
      _        -> Error [error]

toLocation :: String -> Report String
toLocation = Success . map toLower

toStarred :: String -> Report Bool
toStarred text =
  let error = "'" ++ text ++ "' not one of Y or ''."
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

toMetadata :: [String] -> Report Metadata
toMetadata lines' =
    let title  = expectCells 1 (lines' !! 1) >>= toTitle . head
        author = expectCells 1 (lines' !! 2) >>= toAuthor . head
    in  Metadata <$> author <*> title

between :: Eq a => a -> a -> [a] -> [a]
between start end =
  takeWhile (/= end) . drop 1 . dropWhile (/= start)

inParentheses :: String -> String
inParentheses = between '(' ')'

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

expectCells :: Int -> String -> Report [String]
expectCells expected text =
  let cells = columns text in
  expectLength "attribute" "attributes" expected cells >> return cells

expectLength :: String -> String -> Int -> [a] -> Report ()
expectLength what whats expected items =
  let
    length' = length items
    error =
      "Expected " ++ show expected ++ " "
        ++ (if expected == 1 then what else whats) ++ " but found "
        ++ show length' ++ "."
  in
    if length' == expected
    then Success ()
    else Error [error]

readExport :: FilePath -> IO (Maybe Export)
readExport path =
  readFile path >>= \text ->
    let
      lines' = lines text
      preambleLength = 8
      preambleLines = take preambleLength lines'
      annotationLines = drop preambleLength lines'
      preambleError =
        "Parse error in preamble on lines 1 through " ++ show preambleLength ++ "."
      annotationError line =
        "Parse error in annotation on line " ++ show (line + preambleLength) ++ "."
      annotations =
        (sequenceA . map (\(line, text) ->
          (labelAndItemize (annotationError line) . toAnnotation) text))
          (zip [1..] annotationLines)
      metadata =
        labelAndItemize preambleError
          (expectLength "line" "lines" preambleLength preambleLines >> toMetadata preambleLines)
      report = Export <$> metadata <*> annotations
    in
      case report of
        Success export ->
          return (Just export)
        Error errors ->
          mapM_ putStrLn errors >> return Nothing

printExport :: FilePath -> IO ()
printExport path =
  readExport path >>= \export ->
    let
      toReadable =
        \(number, annotation) -> show number ++ ": " ++ (elide . excerpt) annotation
    in
      case export of
        Just (Export (Metadata author title) annotations) ->
             putStrLn ("Excerpts from '" ++ title ++ "' by '" ++ author ++ "'.")
          >> (mapM_ putStrLn . map toReadable) (zip [1..] annotations)
        Nothing -> return ()
