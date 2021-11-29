module Hal.Parser.AtomicParsing
  ( parseAtomicValues,
  )
where

import Data.Char (isAlphaNum)
import Hal.Parser.Datum
  ( Datum (Bool, Char, Float, Number, String, Symbol),
  )
import Hal.Parser.Utils (strObject)
import LibParserCombinator
  ( Alternative ((<|>)),
    Parser,
    binary,
    char,
    digit,
    eof,
    float,
    get,
    hexadecimal,
    integer,
    letter,
    many,
    noneOf,
    octal,
    oneOf,
    quoted,
    satisfy,
    string,
  )

---- Utils ----

-- valid symbol as a name in scheme
symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

-- valid identifier characters in scheme
validChar :: Parser Char
validChar = symbol <|> letter

-- parse special characters
escapedChar :: Parser Char
escapedChar =
  char '\\'
    >> ( oneOf "\\\""
           <|> strObject "n" '\n'
           <|> strObject "r" '\r'
           <|> strObject "t" '\t'
       )

-- get a decimal scheme number
getDecimal :: Parser Datum
getDecimal =
  Number <$> integer
    <|> (string "#d" >> Number <$> integer)

-- get a hexadecimal scheme number
getHex :: Parser Datum
getHex = string "#x" >> Number <$> hexadecimal

-- get an octal scheme number
getOct :: Parser Datum
getOct = string "#o" >> Number <$> octal

-- get a binary scheme number
getBin :: Parser Datum
getBin = string "#b" >> Number <$> binary

---- Atomic Parsers ----

-- get a scheme boolean
getBool :: Parser Datum
getBool = strObject "#t" (Bool True) <|> strObject "#f" (Bool False)

-- get a scheme atom
getSymbol :: Parser Datum
getSymbol =
  Symbol
    <$> ( ((:) <$> validChar)
            <*> many (validChar <|> digit)
        )

-- get a scheme string
getString :: Parser Datum
getString = quoted $ String <$> many (escapedChar <|> noneOf "\"\\")

-- get a scheme number
getNumber :: Parser Datum
getNumber = getDecimal <|> getHex <|> getOct <|> getBin

-- parse a scheme character
getSpecialChars :: Parser Char
getSpecialChars = strObject "#\\alarm" '\x0007'
    <|> strObject "#\\backspace" '\x0008'
    <|> strObject "#\\delete" '\x007F'
    <|> strObject "#\\esc" '\x001B'
    <|> strObject "#\\newline" '\x000A'
    <|> strObject "#\\page" '\x000C'
    <|> strObject "#\\return" '\x000D'
    <|> strObject "#\\space" ' '
    <|> strObject "#\\tab" '\t'
    <|> strObject "#\\vtab" '\v'

getCharacter :: Parser Datum
getCharacter =
  Char
    <$> ( getSpecialChars
            <|> (string "#\\" >> get <* eof)
            <|> (string "#\\" >> satisfy (not . isAlphaNum))
        )

-- parse a scheme float
getFloat :: Parser Datum
getFloat = Float <$> float

---- Parser ----

-- parse an atomic value
parseAtomicValues :: Parser Datum
parseAtomicValues =
  getBool
    <|> getCharacter
    <|> getFloat
    <|> getNumber
    <|> getSymbol
    <|> getString