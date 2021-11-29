module Hal.Parser.Parser
  ( parseStringToDatums,
  )
where

import Hal.Parser.Datum (Datum (..))
import Hal.Parser.ListParsing (lispParser)
import LibParserCombinator (Parser (parse))

concatList :: Datum -> Datum
concatList (List x) = List (map concatList x)
concatList (DottedList a (List b)) = concatList $ List (a ++ b)
concatList (DottedList a (DottedList b c)) = concatList $ DottedList (a ++ b) c
concatList a = a

concatLists :: [Datum] -> [Datum]
concatLists = map concatList

parseStringToDatums :: String -> Maybe [Datum]
parseStringToDatums input = case parse lispParser input of
  Nothing -> Nothing
  Just (val, "") -> Just $ concatLists val
  Just (val, rest) -> Nothing
