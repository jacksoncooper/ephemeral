module Kindle where

import Data.Char (toLower, toUpper)

import Report (Report(..), labelAndItemize)

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

type Author = String
type Title = String

data Metadata =
  Metadata Author Title
  deriving Show

data Export =
  Export Metadata [Annotation]
  deriving Show

excerpt :: Annotation -> String
excerpt (Highlight _ _ _ excerpt) = excerpt
excerpt (Note _ _ excerpt) = excerpt

elide :: String -> String
elide text =
  let words' = words text
  in
    if length words' > 10
    then (unwords . take 10) words' ++ "…"
    else text

capitalize :: String -> String
capitalize (first : rest) = toUpper first : rest

capitalize' :: String -> String
capitalize' word =
  let
    conjunctions = ["and", "as", "but", "for", "if", "nor", "or", "so", "yet"]
    articles = ["a", "an", "the"]
    prepositions = ["as", "at", "by", "for", "in", "of", "off", "on", "per", "to", "up", "via"]
    ignore = conjunctions ++ articles ++ prepositions
  in
    if word `elem` ignore
    then word
    else capitalize word

title :: String -> String
title text =
  let
    words' = words text
    first : rest = words'
  in
    unwords (capitalize first : map capitalize' rest)

toAuthor :: String -> Report String
toAuthor  = Success . drop 1 . dropWhile (/= ' ')

toTitle :: String -> Report String
toTitle = Success . title . map toLower

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

toExport :: [String] -> Report Export
toExport lines' =
    let
      preambleLength = 8
      preambleLines = take preambleLength lines'
      annotationLines = drop preambleLength lines'
      preambleError =
        "Parse error in preamble on lines 1 through "
          ++ show preambleLength ++ "."
      annotationError number =
        "Parse error in annotation on line "
          ++ show (preambleLength + number) ++ "."
      annotationsReport =
        (sequenceA . map (\(line, text) ->
          (labelAndItemize (annotationError line) . toAnnotation) text))
          (zip [1..] annotationLines)
      metadataReport =
        labelAndItemize preambleError
          (expectLength "line" "lines" preambleLength preambleLines
            >> toMetadata preambleLines)
      in
        Export <$> metadataReport <*> annotationsReport

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

expectCells :: Int -> String -> Report [String]
expectCells expected text =
  let cells = columns text in
  expectLength "attribute" "attributes" expected cells >> return cells

writeExport :: FilePath -> IO (Maybe Export)
writeExport path =
  let log = "kindle.log"
  in
    readFile path >>= \text ->
      case toExport (lines text) of
        Success export ->
          return (Just export)
        Error errors ->
          writeFile log (unlines errors) >> return Nothing

printExport :: FilePath -> IO ()
printExport path =
  writeExport path >>= \export ->
    let
      toReadable = \(number, annotation) ->
        show number ++ ". " ++ (elide . excerpt) annotation
    in
      case export of
        Just (Export (Metadata author title) annotations) ->
          putStrLn ("Excerpts from '" ++ title ++ "' by '" ++ author ++ "':")
            >> (mapM_ putStrLn . map toReadable) (zip [1..] annotations)
        Nothing
          -> putStrLn "Parse error, see 'kindle.log' for details."
