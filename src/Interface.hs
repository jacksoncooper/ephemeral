module Interface
  ( start
  ) where

import Data.List (intercalate)

import Kindle as K
import Word as W

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
        kindleImport >> return ()
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

kindleImport :: IO (Maybe [W.Word])
kindleImport = do
  putStrLn "Please enter the path to the Kindle export."
  path <- prompt
  export <- K.writeExport path
  return (K.toWords <$> export)

prompt :: IO String
prompt = putStr "> " >> getLine
