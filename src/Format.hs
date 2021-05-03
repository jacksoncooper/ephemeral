module Format
 ( elide
 , strip
 , title
 ) where

import Data.Char (isLetter, toLower, toUpper)
import Data.List (dropWhileEnd)
import Prelude hiding (error)

elide :: String -> String
elide text =
  if length words' > 10
  then (unwords . take 10) words' ++ "…"
  else text
  where
    words' = words text

title :: String -> String
title text =
  unwords (capitalize first : map capitalize' rest)
  where
    words' = (words . map toLower) text
    first : rest = words'

strip :: String -> String
strip =
  dropWhileEnd notLetter . dropWhile notLetter
  where
    notLetter = not . isLetter

capitalize :: String -> String
capitalize "" = ""
capitalize (first : rest) = toUpper first : rest

capitalize' :: String -> String
capitalize' word =
  if word `elem` ignore
  then word
  else capitalize word
  where
    conjunctions = ["and", "as", "but", "for", "if", "nor", "or", "so", "yet"]
    articles = ["a", "an", "the"]
    prepositions = ["as", "at", "by", "for", "in", "of", "off", "on", "per", "to", "up", "via"]
    ignore = conjunctions ++ articles ++ prepositions
