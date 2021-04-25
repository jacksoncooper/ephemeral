module Interface
  ( start
  ) where

import Data.List (intercalate)
import Kindle as K
import Word as W
import Storage (save)

type Import = String -> String -> IO (Maybe [W.Word])

data Menu =
    Review
  | Kindle
  | Unknown
  deriving Show

start :: IO ()
start =
  menu >>= \choice ->
    case choice of
      Kindle ->
        doKindleImport
      Review ->
        putStrLn "Not implemented."
      Unknown ->
        putStrLn "Not implemented."

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

doKindleImport :: IO ()
doKindleImport =
  doImport
    "Please enter the path to the CSV file produced by your Kindle."
    "kindle.log"
    K.readWords

doImport :: String -> String -> Import -> IO ()
doImport message log how = do
  putStrLn message
  path <- prompt
  import' <- how log path
  case import' of
    (Just words) -> save words >> putStrLn (success words)
    Nothing -> putStrLn error
  where
    success words = "Successfully imported " ++ show (length words) ++ " words."
    error = "Import failed. Please see '" ++ log ++ "' for details."
