module Interface
  ( start
  ) where

import Data.List (intercalate)

import Kindle (writeExport)
import Word (toWords)

data Menu =
    Review
  | Kindle
  | Unknown
  deriving Show

prompt :: IO String
prompt = putStr "> " >> getLine

start :: IO ()
start =
  menu >>= \choice ->
    case choice of
      Kindle -> do
        putStrLn "Please enter the path to the Kindle export."
        path <- prompt
        export <- writeExport path
        case export of
          Just export' -> return (toWords export') >> return ()
          Nothing -> return ()
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
