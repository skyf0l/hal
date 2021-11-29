module Hal.Parser.ListParsing
  ( parseLists,
    parseDatum,
    lispParser,
    skipSpacesAndComments,
  )
where

import Hal.Parser.AtomicParsing (parseAtomicValues)
import Hal.Parser.Datum (Datum (DottedList, List, Symbol))
import LibParserCombinator
  ( Alternative ((<|>)),
    Parser (Parser),
    char,
    endBy,
    many,
    many1,
    sepBy,
    skip,
    skipMany,
    skipUntil,
    space,
    spaces1,
    string,
    untilNewLineOrEof,
  )

---- Utils ----

-- line comment: ;...\n
lineComment :: Parser ()
lineComment = () <$ string ";" <* untilNewLineOrEof

-- nested block comment: #|...|#
-- error on #|... #|...|# here |#|
blockComment :: Parser ()
blockComment = () <$ string "#|" <* skipUntil (skipSpacesAndComments *> string "|#")

-- datum comment: #; datum
datumComment :: Parser ()
datumComment = () <$ string "#;" <* parseDatum

comment :: Parser ()
comment = lineComment <|> blockComment <|> datumComment

skipSpacesAndComments :: Parser ()
skipSpacesAndComments = skipMany (skip space <|> comment)

---- Non Atomic Parsers ----

-- parse a scheme List
getList :: Parser Datum
getList = List <$> sepBy parseDatum spaces1

-- parse a scheme DottedList
getDottedList :: Parser Datum
getDottedList = do
  head <- endBy parseDatum spaces1
  tail <- char '.' >> spaces1 >> parseDatum
  return $ DottedList head tail

getQuoted :: Parser Datum
getQuoted = do
  x <- string "\'" *> parseDatum
  return $ List [Symbol "quote", x]

getQuasiquoted :: Parser Datum
getQuasiquoted = do
  x <- string "`" *> parseDatum
  return $ List [Symbol "quasiquote", x]

getUnquoted :: Parser Datum
getUnquoted = do
  x <- string "," *> parseDatum
  return $ List [Symbol "unquote", x]

getUnquoteSplicing :: Parser Datum
getUnquoteSplicing = do
  x <- string ",@" *> parseDatum
  return $ List [Symbol "unquote-splicing", x]

getSyntax :: Parser Datum
getSyntax = do
  x <- string "#'" *> parseDatum
  return $ List [Symbol "syntax", x]

getQuasisyntax :: Parser Datum
getQuasisyntax = do
  x <- string "#`" *> parseDatum
  return $ List [Symbol "quasisyntax", x]

getUnsyntax :: Parser Datum
getUnsyntax = do
  x <- string "#," *> parseDatum
  return $ List [Symbol "unsyntax", x]

getUnsyntaxSplicing :: Parser Datum
getUnsyntaxSplicing = do
  x <- string "#,@" *> parseDatum
  return $ List [Symbol "unsyntax-splicing", x]

---- Parsing ----

-- List embeded in parenthesis
parseParenLists :: Parser Datum
parseParenLists =
  char '(' *> skipSpacesAndComments
    *> (getDottedList <|> getList)
    <* skipSpacesAndComments
    <* char ')'

parseLists :: Parser Datum
parseLists =
  getQuoted
    <|> getQuasiquoted
    <|> getUnquoteSplicing
    <|> getUnquoted
    <|> getSyntax
    <|> getQuasisyntax
    <|> getUnsyntaxSplicing
    <|> getUnsyntax
    <|> parseParenLists

parseDatum :: Parser Datum
parseDatum =
  skipSpacesAndComments
    *> (parseLists <|> parseAtomicValues)

lispParser :: Parser [Datum]
lispParser = many parseDatum <* skipSpacesAndComments