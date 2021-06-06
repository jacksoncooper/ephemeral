module Kindle
  ( readWords
  ) where

import Data.Char (toLower)
import Prelude hiding (error, log)

import qualified Excerpt as E
import Export hiding (author, title)
import qualified Format as F
import Parse
import Report

data Color =
    Pink
  | Blue
  | Yellow
  | Orange
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

type Kindle = Export [Annotation]

toAuthor :: String -> Report String
toAuthor  = Success . drop 1 . dropWhile (/= ' ')

toTitle :: String -> Report String
toTitle = Success . F.title

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
toLocation = Success

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
    in Metadata <$> author <*> title

toImport :: [String] -> Report Kindle
toImport lines' =
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

toWords :: Kindle -> [E.Excerpt]
toWords (Export (Metadata author title) annotations) =
  map word annotations
  where
    -- TODO: Note the duplicity of the RHS of the following equations.
    -- No good. You should revisit the annotation type.
    word (Highlight _ location _ excerpt) =
      E.Excerpt author title location excerpt Nothing
    word (Note location _ excerpt) =
      E.Excerpt author title location excerpt Nothing

readKindle :: FilePath -> FilePath -> IO (Maybe Kindle)
readKindle log path =
  -- TODO: readFile throws an exception when given an improper file path.
  readFile path >>= \text ->
    case toImport (lines text) of
      Success export -> return (Just export)
      Error errors -> writeFile log (unlines errors) >> return Nothing

readWords :: FilePath -> FilePath -> IO (Maybe [E.Excerpt])
readWords log path = do
  annotations <- readKindle log path
  return (toWords <$> annotations)
