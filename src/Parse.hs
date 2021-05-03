module Parse where

import Prelude hiding (error)

import Report

between :: Eq a => a -> a -> [a] -> [a]
between start end =
  takeWhile (/= end) . drop 1 . dropWhile (/= start)

inParentheses :: String -> String
inParentheses = between '(' ')'

columns :: String -> [String]
columns = map reverse . columns'

columns' :: String -> [String]
columns' = go False [] '"'
  where
    go :: Bool -> String -> Char -> String -> [String]
    go _ _ _ [] = []
    go consume column toggle (character : rest) =
      if character == toggle then
        let remaining = go (not consume) [] toggle rest in
        if consume
        then column : remaining
        else remaining
      else
        if consume
        then go True (character : column) toggle rest
        else go False [] toggle rest

expectLength :: String -> String -> Int -> [a] -> Report ()
expectLength what whats expected items =
  let
    length' = length items
    error =
      "Expected " ++ show expected ++ " "
        ++ (if expected == 1 then what else whats) ++ " but found "
        ++ show length' ++ "."
  in
    if length' == expected
    then Success ()
    else Error [error]

expectCells :: Int -> String -> Report [String]
expectCells expected text =
  let cells = columns text in
  expectLength "attribute" "attributes" expected cells >> return cells
