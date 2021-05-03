{-# LANGUAGE QuasiQuotes #-}

module Template
  ( writeHTML
  ) where

import Text.Printf (printf)
import Text.RawString.QQ

import qualified Word as W

path :: String
path = "word.html"

template :: String
template = [r|<!DOCTYPE html>
<html>
  <head>
    <meta charset="utf-8">
    <title>%s</title>
  </head>
  <body>
    <p>%s</p>
    <p>From <em>%s</em> by %s (%s).</p>
    <p>
      Define with
      <a href="https://www.merriam-webster.com/dictionary/%s">Merriam-Webster</a> or
      <a href="https://www.google.com/search?q=%s">Oxford Languages</a>.
    </p>
  </body>
</html>
|]

makeHTML :: W.Word -> String
makeHTML (W.Word author title location excerpt _) =
  printf template excerpt excerpt title author location excerpt excerpt

writeHTML :: W.Word -> IO ()
writeHTML word =
    -- TODO: writeFile throws an exception when permissions are wonky.
  writeFile path (makeHTML word)
