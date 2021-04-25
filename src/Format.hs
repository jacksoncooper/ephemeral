module Format
 ( elide
 , title
 ) where

import Data.Char (toLower, toUpper)

elide :: String -> String
elide text =
  let words' = words text
  in
    if length words' > 10
    then (unwords . take 10) words' ++ "…"
    else text

capitalize :: String -> String
capitalize (first : rest) = toUpper first : rest

capitalize' :: String -> String
capitalize' word =
  let
    conjunctions = ["and", "as", "but", "for", "if", "nor", "or", "so", "yet"]
    articles = ["a", "an", "the"]
    prepositions = ["as", "at", "by", "for", "in", "of", "off", "on", "per", "to", "up", "via"]
    ignore = conjunctions ++ articles ++ prepositions
  in
    if word `elem` ignore
    then word
    else capitalize word

title :: String -> String
title text =
  let
    words' = (words . map toLower) text
    first : rest = words'
  in
    unwords (capitalize first : map capitalize' rest)
