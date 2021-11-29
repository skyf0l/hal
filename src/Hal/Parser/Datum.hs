module Hal.Parser.Datum
  ( Datum (..),
    isAtom,
    toFloat,
    toNumber,
  )
where

data Datum
  = Symbol String
  | Number Integer
  | String String
  | Bool Bool
  | Char Char
  | Float Float
  | List [Datum]
  | DottedList [Datum] Datum

instance Show Datum where
  show (Symbol a) = a
  show (Number n) = show n
  show (String s) = "\"" ++ s ++ "\""
  show (Bool True) = "#t"
  show (Bool False) = "#f"
  show (Char '\x0007') = "#\\alarm"
  show (Char '\x0008') = "#\\backspace"
  show (Char '\x007F') = "#\\delete"
  show (Char '\x001B') = "#\\esc"
  show (Char '\x000A') = "#\\newline"
  show (Char '\x000C') = "#\\page"
  show (Char '\x000D') = "#\\return"
  show (Char ' ') = "#\\space"
  show (Char '\t') = "#\\tab"
  show (Char '\v') = "#\\vtab"
  show (Char c) = "#\\" ++ [c]
  show (Float f) = show f
  show (List a) = "(" ++ showDatumList a ++ ")"
  show (DottedList a b) = "(" ++ show a ++ " . " ++ show b ++ ")"

showDatumList :: [Datum] -> String
showDatumList [] = ""
showDatumList [a] = show a
showDatumList (a : al) = show a ++ " " ++ showDatumList al

isAtom :: Datum -> Bool
isAtom (Symbol _) = True
isAtom (Number _) = True
isAtom (String _) = True
isAtom (Bool _) = True
isAtom (Char _) = True
isAtom (Float _) = True
isAtom (List []) = True
isAtom (List _) = False
isAtom (DottedList _ _) = False

toFloat :: Datum -> Datum
toFloat (Number a) = Float (fromInteger a :: Float)
toFloat a = a

toNumber :: Datum -> Datum
toNumber (Float a) = Number $ round a
toNumber a = a