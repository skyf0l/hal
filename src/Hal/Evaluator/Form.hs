module Hal.Evaluator.Form
  ( Form (..),
    Formal (..),
    Constant (..),
    Clause (..),
    Expression (..),
    Definition (..),
  )
where

import Data.List.NonEmpty (NonEmpty)
import Hal.Parser.Datum (Datum)

data Expression
  = If2 Expression Expression
  | If3 Expression Expression Expression
  | Cond (NonEmpty Clause)
  | Application Expression [Expression]
  | Lambda Formal (NonEmpty Expression)
  | BeginExpr [Expression]
  | Quote Datum
  | Set String Expression
  | Const Constant
  | Var String
  | Load Expression

data Definition
  = Define String (Maybe Expression)
  | BeginDef [Definition]

data Form
  = Definition Definition
  | Expression Expression

data Clause
  = Clause Expression [Expression]
  | Else [Expression]

data Constant
  = Bool Bool
  | Number Integer
  | Float Float
  | String String
  | Char Char

data Formal
  = Exact [String]
  | Variadic [String] String

instance Show Form where
  show (Definition def) = show def
  show (Expression expr) = show expr

instance Show Definition where
  show (Define name expr) = "define " ++ name ++ " " ++ show expr
  show (BeginDef defs) = "begin " ++ unwords (map show defs)

instance Show Expression where
  show (If2 a b) = "(if " ++ show a ++ " then " ++ show b ++ ")"
  show (If3 a b c) = "(if " ++ show a ++ " then " ++ show b ++ " else " ++ show c ++ ")"
  show (Cond clauses) = "(cond " ++ show clauses ++ ")"
  show (Application a b) = "(Application " ++ show a ++ " " ++ unwords (map show b) ++ ")"
  show (Lambda a b) = "(lambda " ++ show a ++ " " ++ show b ++ ")"
  show (BeginExpr expr) = "begin " ++ unwords (map show expr)
  show (Quote a) = "(quote " ++ show a ++ ")"
  show (Set a b) = "(set " ++ a ++ " " ++ show b ++ ")"
  show (Const a) = show a
  show (Var a) = a
  show (Load a) = "(load " ++ show a ++ ")"

instance Show Constant where
  show (Bool a) = show a
  show (Number a) = show a
  show (Float a) = show a
  show (String a) = show a
  show (Char a) = show a

instance Show Formal where
  show (Exact a) = "(" ++ unwords a ++ ")"
  show (Variadic [] b) = b
  show (Variadic a b) = "(" ++ unwords a ++ " . " ++ b ++ ")"

instance Show Clause where
  show (Clause a b) = "(" ++ show a ++ " " ++ unwords (map show b) ++ ")"
  show (Else a) = "(else " ++ unwords (map show a) ++ ")"