module Interface
  ( start
  ) where

import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Time.Calendar (Day, diffDays)
import Data.Time.Clock (UTCTime(..), getCurrentTime)
import System.Environment (getArgs)
import System.IO (BufferMode(NoBuffering), stdout, hSetBuffering)
import System.Random (randomRIO)

import qualified Format as F
import qualified Kindle as K
import qualified Storage as S
import qualified Template as T
import qualified Word as W

type Import = String -> String -> IO (Maybe [W.Word])

data Menu =
    Review
  | Kindle
  | Exit
  | Unknown
  deriving Show

start :: IO ()
start = do
  hSetBuffering stdout NoBuffering
  existing <- S.load
  let words' = fromMaybe [] existing
  arguments <- getArgs
  case (length arguments) of
    0 -> manage words'
    1 ->
      if argument == "--select"
      then select words'
      else putStrLn ("Unknown argument, expected --select.")
      where
        argument = arguments !! 0
    _ ->
      putStrLn "Expected one argument."

select :: [W.Word] -> IO ()
select words' =
  if length words' > 0
  then do
    (UTCTime now _) <- getCurrentTime
    (word, updated) <- select' [] now words'
    S.save updated
    maybe
      (putStrLn "All words reviewed, come back later or import some fresh ones.")
      T.writeHTML
      word
  else
    putStrLn "Cannot --select from no words."

select' :: [W.Word] -> Day -> [W.Word] -> IO (Maybe W.Word, [W.Word])
select' visited _ [] = return (Nothing, visited)
select' visited now words' =
  randomRIO (1, length words') >>= \size ->
    let
      (including, rest) = splitAt size words'
      excluding = init including
      unvisited = excluding ++ rest
      word = last including
      seen = word { W.seen = Just now }
      tryAgain = select' (seen : visited) now unvisited
      foundIt = return (Just seen, seen : visited ++ unvisited)
    in
      case W.seen word of
        Just day ->
          if diffDays now day > 21
          then foundIt
          else tryAgain
        Nothing -> foundIt

manage :: [W.Word] -> IO ()
manage words' = do
  choice <- menu
  case choice of
    Review  -> putStrLn "Not implemented."
    Kindle  -> doKindleImport words'
    Exit    -> return ()
    Unknown -> start

menu :: IO Menu
menu =
  doChoice >>= \choice ->
    case choice of
      "1" -> return Review
      "2" -> return Kindle
      "3" -> return Exit
      _   -> return Unknown
  where
    doChoice = putStrLn message >> prompt
    message = intercalate "\n" $
      [ "Please select from the following options."
      , "  [1] Review."
      , "  [2] Import from Kindle."
      , "  [3] Exit."
      ]

prompt :: IO String
prompt = putStr "> " >> getLine

doKindleImport :: [W.Word] -> IO ()
doKindleImport =
  doImport
    "Please enter the path to the CSV file produced by your Kindle."
    "kindle.log"
    K.readWords

doImport :: String -> String -> Import -> [W.Word] -> IO ()
doImport message log' how existing = do
  putStrLn message
  path <- prompt
  import' <- how log' path
  case import' of
    (Just excerpts) ->
      -- TODO: This sort of concatenation is really inefficient and requires
      -- traversing the entirety of the existing words.
      S.save (existing ++ words') >> putStrLn (success words')
      where
        words' = vocabulary excerpts
    Nothing ->
      putStrLn error'
  where
    success words' = "Successfully imported " ++ show (length words') ++ " words."
    error' = "Import failed. Please see '" ++ log' ++ "' for details."

vocabulary :: [W.Word] -> [W.Word]
vocabulary =
  (map stripWord) . filter smallWord
  where
    smallWord word =
      length ((words . W.excerpt) word) <= 3
    stripWord word@(W.Word { W.excerpt = excerpt })
      = word { W.excerpt = F.strip excerpt }
