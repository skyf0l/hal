module Hal.Parser.Utils
  ( strObject,
  )
where

import LibParserCombinator (Parser, string)

-- parse a string and convert it to a const object
strObject :: String -> a -> Parser a
strObject str obj = string str >> return obj