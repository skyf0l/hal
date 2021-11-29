module Hal.Evaluator.LispVal
  ( LispVal (..),
    Procedure (..),
    NativeProcedure,
    isAtom,
  )
where

import Data.List.NonEmpty (NonEmpty)
import Hal.Evaluator.Environment (EnvPrototype)
import Hal.Evaluator.Form (Expression, Formal)

type NativeProcedure =
  [LispVal] ->
  EnvPrototype LispVal ->
  IO (LispVal, EnvPrototype LispVal)

data LispVal
  = Pair LispVal LispVal
  | Number Integer
  | Float Float
  | String String
  | Bool Bool
  | Char Char
  | Procedure Procedure
  | Symbol String
  | Empty
  | Void

data Procedure
  = NativeP NativeProcedure
  | LambdaP Formal (NonEmpty Expression)

instance Show LispVal where
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
  show (Pair (Symbol "quote") (Pair b Empty)) = "'" ++ show b
  show (Pair (Symbol "quasiquote") (Pair v Empty)) = "`" ++ show v
  show (Pair (Symbol "unquote") (Pair v Empty)) = "," ++ show v
  show (Pair (Symbol "unquote-splicing") (Pair v Empty)) = ",@" ++ show v
  show (Pair (Symbol "syntax") (Pair v Empty)) = "#'" ++ show v
  show (Pair (Symbol "quasisyntax") (Pair v Empty)) = "#`" ++ show v
  show (Pair (Symbol "unsyntax") (Pair v Empty)) = "#," ++ show v
  show (Pair (Symbol "unsyntax-splicing") (Pair v Empty)) = "#,@" ++ show v
  show (Pair car cdr) = "(" ++ show car ++ expandPair cdr ++ ")"
  show Empty = "()"
  show (Procedure _) = "#<procedure>"
  show (Symbol s) = s
  show Void = ""

instance Eq LispVal where
  (==) (Number a) (Number b) = a == b
  (==) (Bool a) (Bool b) = a == b
  (==) (Char a) (Char b) = a == b
  (==) (Float a) (Float b) = a == b
  (==) (Symbol a) (Symbol b) = a == b
  (==) Empty Empty = True
  (==) Void Void = True
  (==) _ _ = False

expandPair :: LispVal -> String
expandPair Empty = ""
expandPair (Pair car' cdr') = " " ++ show car' ++ expandPair cdr'
expandPair d = " . " ++ show d

showLispValList :: [LispVal] -> String
showLispValList [] = ""
showLispValList [a] = show a
showLispValList (a : al) = show a ++ " " ++ showLispValList al

isAtom :: LispVal -> Bool
isAtom (Pair _ _) = False
isAtom _ = True