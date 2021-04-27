module Interface
  ( start
  ) where

import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import qualified Format as F
import qualified Kindle as K
import qualified Storage as S
import qualified Word as W

type Import = String -> String -> IO (Maybe [W.Word])

data Menu =
    Review
  | Kindle
  | Unknown
  deriving Show

start :: IO ()
start = do
  existing <- S.load
  let words' = fromMaybe [] existing
  choice <- menu
  case choice of
    Review  -> putStrLn "Not implemented."
    Kindle  -> doKindleImport words'
    Unknown -> start

menu :: IO Menu
menu =
  doChoice >>= \choice ->
    case choice of
      "1" -> return Review
      "2" -> return Kindle
      _   -> return Unknown
  where
    doChoice = putStrLn message >> prompt
    message = intercalate "\n" $
      [ "Please select from the following options."
      , "  [1] Review."
      , "  [2] Import from Kindle."
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
doImport message log how existing = do
  putStrLn message
  path <- prompt
  import' <- how log path
  case import' of
    (Just excerpts) ->
      -- TODO: This sort of concatenation is really inefficient and requires
      -- traversing the entirety of the existing words.
      S.save (existing ++ words') >> putStrLn (success words')
      where
        words' = vocabulary excerpts
    Nothing ->
      putStrLn error
  where
    success words = "Successfully imported " ++ show (length words) ++ " words."
    error = "Import failed. Please see '" ++ log ++ "' for details."

vocabulary :: [W.Word] -> [W.Word]
vocabulary =
  (map stripWord) . filter smallWord
  where
    smallWord word =
      length ((words . W.excerpt) word) <= 3
    stripWord word@(W.Word { W.excerpt = excerpt })
      = word { W.excerpt = F.strip excerpt }
