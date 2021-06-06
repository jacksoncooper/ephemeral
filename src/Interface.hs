module Interface
  ( start
  ) where

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

type Import = FilePath -> IO (Maybe [E.Excerpt])

start :: IO ()
start = do
  hSetBuffering stdout NoBuffering
  excerpts <- fromMaybe [] <$> S.load
  arguments <- getArgs
  case (length arguments) of
    1 ->
      case (arguments !! 0) of
        "--select" -> select excerpts
        _ -> putStrLn expectFlag
      where
        expectFlag = "error: Unexpected flag. Expected --select."
    2 ->
      case flag of
        "--kindle-import" -> doKindleImport excerpts path
        _ -> putStrLn badFlag
      where
        flag = arguments !! 0
        path = arguments !! 1
        badFlag = "error: Unexpected flag '" ++ flag ++ "'."
    _ ->
      putStrLn "usage: ephemeral [ --select | --kindle-import <path> ]"

select :: [E.Excerpt] -> IO ()
select excerpts =
  if length excerpts > 0
  then do
    (UTCTime now _) <- getCurrentTime
    (excerpt, updated) <- select' [] now excerpts
    S.save updated
    maybe
      (putStrLn "All excerpts reviewed, come back later.")
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

doKindleImport :: [E.Excerpt] -> FilePath -> IO ()
doKindleImport = doImport (K.readWords "kindle.log")

doImport :: Import -> [E.Excerpt] -> FilePath -> IO ()
doImport how existing path = do
  new <- how path
  case new of
    (Just excerpts) ->
      let filtered = vocabulary excerpts
      in S.save (existing ++ filtered) >> putStrLn (success filtered)
    Nothing ->
      putStrLn error
  where
    success excerpts = "Successfully imported " ++ show (length excerpts) ++ " words."
    error = "Import failed. Please see the log file corresponding to the import method."

vocabulary :: [E.Excerpt] -> [E.Excerpt]
vocabulary =
  (map stripWord) . filter smallExcerpt
  where
    smallExcerpt E.Excerpt { E.excerpt = text } =
      length (words text) <= 3
    stripWord excerpt@(E.Excerpt { E.excerpt = text }) =
      excerpt { E.excerpt = F.strip text }
