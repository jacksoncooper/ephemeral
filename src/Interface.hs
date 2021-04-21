module Interface
  ( start
  ) where

import Data.List (intercalate)

data Menu = Review | Kindle
  deriving Show

start :: IO ()
start = menu >>= \choice -> print choice

menu :: IO Menu
menu =
  doChoice >>= \choice ->
    case choice of
      "1" -> return Review
      "2" -> return Kindle
  where
    doChoice = putStrLn prompt >> putStr "> " >> getLine
    prompt = intercalate "\n" $
      [ "Please select from the following options."
      , "  [1] Review."
      , "  [2] Import from Kindle."
      ]
