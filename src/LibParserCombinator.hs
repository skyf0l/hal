{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

-- Parser Combinators library
-- https://hackage.haskell.org/package/base-4.15.0.0/docs/Text-ParserCombinators-ReadP.html

-- https://two-wrongs.com/parser-combinators-parsing-for-haskell-beginners

module LibParserCombinator
  ( module LibParserCombinator,
    module Control.Applicative,
  )
where

import Control.Applicative (Alternative ((<|>)), empty)
import Control.Monad (MonadPlus (..), join, replicateM)
import Data.Char (digitToInt, isAlpha, isDigit, isHexDigit, isLower, isOctDigit, isUpper)
import Data.Foldable (Foldable (foldl'))
import Data.Functor (($>))
import Numeric (readFloat, readHex, readOct)

newtype Parser a = Parser {parse :: String -> Maybe (a, String)}

runParser :: Parser a -> String -> Maybe a
runParser p s = case parse p s of
  Just (a, "") -> Just a
  _ -> Nothing

-- Instances

unit :: a -> Parser a
unit a = Parser (\s -> Just (a, s))

bind :: Parser a -> (a -> Parser b) -> Parser b
bind p f = Parser $ \s -> case parse p s of
  Nothing -> Nothing
  Just (a, s') -> parse (f a) s'

instance Functor Parser where
  fmap f (Parser cs) = Parser $ \s -> case cs s of
    Just (a, s') -> Just (f a, s')
    Nothing -> Nothing

instance Applicative Parser where
  pure = return
  (<*>) p1 p2 = do
    a1 <- p1
    a1 <$> p2

instance Monad Parser where
  return = unit
  (>>=) = bind

instance Alternative Parser where
  (<|>) p1 p2 = Parser $ \s -> case parse p1 s of
    Nothing -> parse p2 s
    Just x -> Just x

-- Always fails
pFail :: Parser a
pFail = Parser $ const Nothing

-- Get one character
get :: Parser Char
get = Parser $ \case
  (c : cs) -> Just (c, cs)
  [] -> Nothing

-- Look-ahead: returns the part of the input that is left, without consuming it
look :: Parser String
look = Parser $ \case
  (c : cs) -> Just (c : cs, cs)
  [] -> Nothing

-- Parses and returns the specified character
char :: Char -> Parser Char
char c = Parser $ \case
  (c' : cs) | c == c' -> Just (c', cs)
  _ -> Nothing

-- Parses and returns the specified string
string :: String -> Parser String
string = foldr (\c -> (<*>) ((:) <$> char c)) (pure [])

-- Consumes and returns the next character
-- if it satisfies the specified predicate
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \case
  (c : cs) | p c -> Just (c, cs)
  _ -> Nothing

-- Parses zero or more occurrences of the given parser
many :: Parser a -> Parser [a]
many p = many1 p <|> pure []

-- Parses one or more occurrences of the given parser
many1 :: Parser a -> Parser [a]
many1 p = (:) <$> p <*> many p

-- Parses n or more occurrences of the given parser
manyN :: Int -> Parser a -> Parser [a]
manyN = replicateM

-- Consumes and returns the next character
-- if it satisfies the specified predicates
oneOf :: String -> Parser Char
oneOf s = satisfy (`elem` s)

noneOf :: String -> Parser Char
noneOf s = satisfy (`notElem` s)

-- Chainl p op x parses zero or more occurrences of p, separated by op
-- Returns a value produced by a left associative application of all functions
-- returned by op
-- If there are no occurrences of p, x is returned
chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op x = chainl1 p op <|> return x

-- Like chainl, but parses one or more occurrences of p
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = do
  x <- p
  rest x
  where
    rest x =
      ( do
          f <- op
          y <- p
          rest (f x y)
      )
        <|> return x

-- Option x p will either parse p or return x without consuming any input
option :: a -> Parser a -> Parser a
option x p = p <|> pure x

-- Parses the first zero or more characters satisfying the predicate
-- Always succeeds, exactly once having consumed all the characters Hence
-- NOT the same as (many (satisfy p))
munch :: (Char -> Bool) -> Parser String
munch p = (:) <$> satisfy p <*> munch p <|> pure []

-- Parses the first one or more characters satisfying the predicate
-- Fails if none, else succeeds exactly once having consumed all the characters
-- Hence NOT the same as (many1 (satisfy p))
munch1 :: (Char -> Bool) -> Parser String
munch1 p = (:) <$> satisfy p <*> munch p <|> pFail

-- Skip parser
skip :: Parser a -> Parser ()
skip p = () <$ p

-- Skip parser multiple times
skipMany :: Parser a -> Parser ()
skipMany p = skip $ many p

skipUntil :: Parser a -> Parser ()
skipUntil end = skip end <|> (skip get *> skipUntil end)

-- Common char patterns

-- \$ (Succeeds if we are at the end of input)
eof :: Parser ()
eof = Parser $ \case
  [] -> Just ((), [])
  _ -> Nothing

-- \d
digit :: Parser Char
digit = satisfy isDigit

-- \s
space :: Parser Char
space = oneOf " \t\n\r"

-- \s*
spaces :: Parser String
spaces = many space

-- \s+
spaces1 :: Parser String
spaces1 = many1 space

-- [a-z]
lower :: Parser Char
lower = satisfy isLower

-- [A-Z]
upper :: Parser Char
upper = satisfy isUpper

-- [a-zA-Z]
letter :: Parser Char
letter = satisfy isAlpha

-- (?:\s*)
skipSpaces :: Parser ()
skipSpaces = skipMany space

-- (?:.*$)
untilNewLineOrEof :: Parser String
untilNewLineOrEof = munch (not . (`elem` "\n"))

sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy parser sep = ((:) <$> parser <*> many (sep *> parser)) <|> pure []

sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 parser sep = (:) <$> parser <*> many (sep *> parser)

endBy :: Parser a -> Parser sep -> Parser [a]
endBy parser sep = many (parser <* sep)

endBy1 :: Parser a -> Parser sep -> Parser [a]
endBy1 parser sep = many1 (parser <* sep)

-- .*(?:[^\s]*)
token :: Parser a -> Parser a
token p = p <* skipSpaces

-- .*(?:[^\s]*)
reserved :: String -> Parser String
reserved s = token $ string s

-- \(.*\)
parens :: Parser a -> Parser a
parens m = reserved "(" *> m <* reserved ")"

-- \".*\"
quoted :: Parser a -> Parser a
quoted m = string "\"" *> m <* string "\""

-- \d+ as integer
unsignedInteger :: Parser Integer
unsignedInteger = read <$> munch1 isDigit

-- \-?\d+ as integer
integer :: Parser Integer
integer = read <$> i
  where
    i = (++) <$> option "" (string "-") <*> munch1 isDigit

-- [\dabcdefABCDEF]+ as integer
hexadecimal :: Parser Integer
hexadecimal = fst . head . readHex <$> i
  where
    i =
      (++) <$> option "" (string "-")
        <*> munch1 isHexDigit

-- [01234567]+ as integer
octal :: Parser Integer
octal = fst . head . readOct <$> i
  where
    i =
      (++) <$> option "" (string "-")
        <*> munch1 isOctDigit

-- convert string to binary number
readBin :: String -> Int
readBin = foldl' (\acc x -> acc * 2 + digitToInt x) 0

-- [01]+ as integer
binary :: Parser Integer
binary = toInteger . readBin <$> i
  where
    i =
      (++) <$> option "" (string "-")
        <*> munch1 isOctDigit

-- (\-?\d+)|(\-?\d+\.\d+) as float
float :: Parser Float
float = read <$> fs
  where
    sign = option "" (string "-")
    entire = option "0" (token $ munch1 isDigit)
    dot = string "."
    fraction = munch1 isDigit
    f = concat3 <$> entire <*> dot <*> fraction
    fs = (++) <$> sign <*> f

-- (\-?\d+)|(\-?\d+\.\d+) as double
double :: Parser Double
double = fst . head . readFloat <$> i
  where
    i =
      (++) <$> option "" (string "-")
        <*> (join <$> sequence [many1 digit, (: []) <$> char '.', many1 digit])

-- multi concat
concat3 :: String -> String -> String -> String
concat3 a b c = a ++ b ++ c

-- \d*\.?\d* as float
-- 1 or 1.0 or .0 or 1 . 0 or .
unsignedFloat :: Parser Float
unsignedFloat = read <$> f <|> fromIntegral <$> unsignedInteger
  where
    entire = option "0" (token $ munch1 isDigit)
    dot = token $ string "."
    fraction = option "0" (munch1 isDigit)
    f = concat3 <$> entire <*> dot <*> fraction