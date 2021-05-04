module Interface
  ( start
  ) where

import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Time.Calendar (Day, diffDays)
import Data.Time.Clock (UTCTime(..), getCurrentTime)
import Prelude hiding (error, log)
import System.Environment (getArgs)
import System.IO (BufferMode(NoBuffering), stdout, hSetBuffering)
import System.Random (randomRIO)

import qualified Excerpt as E
import qualified Format as F
import qualified Kindle as K
import qualified Storage as S
import qualified Template as T

type Import = String -> String -> IO (Maybe [E.Excerpt])

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
  let excerpts = fromMaybe [] existing
  arguments <- getArgs
  case (length arguments) of
    0 -> manage excerpts
    1 ->
      if argument == "--select"
      then select excerpts
      else putStrLn ("Unknown argument, expected --select.")
      where
        argument = arguments !! 0
    _ ->
      putStrLn "Expected one argument."

select :: [E.Excerpt] -> IO ()
select excerpts =
  if length excerpts > 0
  then do
    (UTCTime now _) <- getCurrentTime
    (excerpt, updated) <- select' [] now excerpts
    S.save updated
    maybe
      (putStrLn "All excerpts reviewed, come back later or import some fresh ones.")
      T.writeHTML
      excerpt
  else
    putStrLn "Cannot --select from no excerpts."

select' :: [E.Excerpt] -> Day -> [E.Excerpt] -> IO (Maybe E.Excerpt, [E.Excerpt])
select' visited _ [] = return (Nothing, visited)
select' visited now excerpts =
  randomRIO (1, length excerpts) >>= \size ->
    let
      (including, rest) = splitAt size excerpts
      excluding = init including
      unvisited = excluding ++ rest
      excerpt = last including
      seen = excerpt { E.seen = Just now }
      tryAgain = select' (seen : visited) now unvisited
      foundIt = return (Just seen, seen : visited ++ unvisited)
    in
      case E.seen excerpt of
        Just day ->
          if diffDays now day > 21
          then foundIt
          else tryAgain
        Nothing -> foundIt

manage :: [E.Excerpt] -> IO ()
manage excerpts = do
  choice <- menu
  case choice of
    Review  -> putStrLn "Not implemented."
    Kindle  -> doKindleImport excerpts
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

doKindleImport :: [E.Excerpt] -> IO ()
doKindleImport =
  doImport
    "Please enter the path to the CSV file produced by your Kindle."
    "kindle.log"
    K.readWords

doImport :: String -> String -> Import -> [E.Excerpt] -> IO ()
doImport message log how existing = do
  putStrLn message
  path <- prompt
  collection <- how log path
  case collection of
    (Just excerpts) ->
      let filtered = vocabulary excerpts in
      S.save (existing ++ filtered) >> putStrLn (success filtered)
    Nothing ->
      putStrLn error
  where
    success excerpts = "Successfully imported " ++ show (length excerpts) ++ " words."
    error = "Import failed. Please see '" ++ log ++ "' for details."

vocabulary :: [E.Excerpt] -> [E.Excerpt]
vocabulary =
  (map stripWord) . filter smallExcerpt
  where
    smallExcerpt E.Excerpt { E.excerpt = text } =
      length (words text) <= 3
    stripWord excerpt@(E.Excerpt { E.excerpt = text })
      = excerpt { E.excerpt = F.strip text }
