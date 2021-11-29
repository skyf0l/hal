module Hal.Evaluator.DatumToForm
  ( datumsToForms,
  )
where

import Control.Applicative (Alternative ((<|>)))
import Data.List.NonEmpty (nonEmpty)
import Hal.Evaluator.Form
  ( Clause (..),
    Constant (..),
    Definition (..),
    Expression (..),
    Form (..),
    Formal (..),
  )
import Hal.Parser.Datum (Datum (..))

-- Return a list of symbol into string or nothing
checkListParam :: [Datum] -> Maybe [String]
checkListParam [] = Just []
checkListParam (x : xs) = case x of
  Symbol x -> case checkListParam xs of
    Just xs' -> Just (x : xs')
    _ -> Nothing
  _ -> Nothing

-- Return a formal
getFormal :: Datum -> Maybe Formal
getFormal (List x) = case checkListParam x of
  Just xs -> Just (Exact xs)
  _ -> Nothing
getFormal (DottedList x (Symbol y)) = case checkListParam x of
  Just xs -> Just (Variadic xs y)
  _ -> Nothing
getFormal (Symbol y) = Just (Variadic [] y)
getFormal _ = Nothing

-- Define command

datumToDefineLambdaExact :: Datum -> Maybe Definition
datumToDefineLambdaExact
  (List (Symbol "define" : List (Symbol var : params) : body)) =
    case (checkListParam params, datumsToExprs body) of
      (Just p, Just b) -> case nonEmpty b of
        Just nb -> Just $ Define var $ Just (Lambda (Exact p) nb)
        _ -> Nothing
      _ -> Nothing
datumToDefineLambdaExact _ = Nothing

datumToDefineLambdaVariadic :: Datum -> Maybe Definition
datumToDefineLambdaVariadic
  (List (Symbol "define" : DottedList (Symbol var : params) (Symbol v) : body)) =
    case (checkListParam params, datumsToExprs body) of
      (Just p, Just b) -> case nonEmpty b of
        Just nb -> Just $ Define var $ Just (Lambda (Variadic p v) nb)
        _ -> Nothing
      _ -> Nothing
datumToDefineLambdaVariadic _ = Nothing

datumToDefine :: Datum -> Maybe Definition
datumToDefine (List [Symbol "define", Symbol a, b]) = case datumToExpr b of
  x@(Just _) -> Just $ Define a x
  _ -> Nothing
datumToDefine (List [Symbol "define", Symbol a]) =
  Just $ Define a Nothing
datumToDefine d =
  datumToDefineLambdaExact d
    <|> datumToDefineLambdaVariadic d
    <|> Nothing

-- begin definition
datumToBeginDef :: Datum -> Maybe Definition
datumToBeginDef (List (Symbol "begin" : xs)) = case datumsToDefs xs of
  Just xs' -> Just $ BeginDef xs'
  _ -> Nothing
datumToBeginDef _ = Nothing

-- constant expression
datumToConst :: Datum -> Maybe Constant
datumToConst (Hal.Parser.Datum.Bool n) = Just $ Hal.Evaluator.Form.Bool n
datumToConst (Hal.Parser.Datum.Number n) = Just $ Hal.Evaluator.Form.Number n
datumToConst (Hal.Parser.Datum.Float n) = Just $ Hal.Evaluator.Form.Float n
datumToConst (Hal.Parser.Datum.String s) = Just $ Hal.Evaluator.Form.String s
datumToConst (Hal.Parser.Datum.Char c) = Just $ Hal.Evaluator.Form.Char c
datumToConst _ = Nothing

-- begin expression
datumToBeginExpr :: Datum -> Maybe Expression
datumToBeginExpr (List (Symbol "begin" : xs)) = case datumsToExprs xs of
  Just xs' -> Just $ BeginExpr xs'
  _ -> error "ok"
datumToBeginExpr _ = Nothing

-- quote expression
datumToQuote :: Datum -> Maybe Datum
datumToQuote (List [Symbol "quote", d]) = Just d
datumToQuote _ = Nothing

-- if expression
datumToIf :: Datum -> Maybe Expression
datumToIf (List [Symbol "if", a, b]) =
  case (datumToExpr a, datumToExpr b) of
    (Just x, Just y) -> Just $ If2 x y
    _ -> Nothing
datumToIf (List [Symbol "if", a, b, c]) =
  case (datumToExpr a, datumToExpr b, datumToExpr c) of
    (Just x, Just y, Just z) -> Just $ If3 x y z
    _ -> Nothing
datumToIf _ = Nothing

-- cond expression
getClause :: Datum -> Maybe Clause
getClause (List (Symbol "else" : xs)) = case datumsToExprs xs of
  Just xs' -> Just $ Else xs'
  _ -> Nothing
getClause (List (test : expr)) = case (datumToExpr test, datumsToExprs expr) of
  (Just t, Just e) -> Just $ Clause t e
  _ -> Nothing
getClause _ = Nothing

checkClause :: [Clause] -> Maybe [Clause]
checkClause [] = Nothing
checkClause [x] = Just [x]
checkClause (x : xs) = case x of
  (Else _) -> Nothing
  x' -> case checkClause xs of
    Just xs' -> Just (x' : xs')
    _ -> Nothing

getClauses :: [Datum] -> Maybe [Clause]
getClauses [] = Just []
getClauses (x : xs) = case getClause x of
  Just x -> case getClauses xs of
    Just xs -> checkClause $ x : xs
    _ -> Nothing
  _ -> Nothing

datumToCond :: Datum -> Maybe Expression
datumToCond (List [Symbol "cond"]) = Nothing
datumToCond (List (Symbol "cond" : c)) = case getClauses c of
  Just c' -> case nonEmpty c' of
    Just c'' -> Just $ Cond c''
    _ -> Nothing
  _ -> Nothing
datumToCond _ = Nothing

-- set expression
datumToSet :: Datum -> Maybe Expression
datumToSet (List [Symbol "set!", Symbol a, b]) = case datumToExpr b of
  Just x -> Just $ Set a x
  _ -> Nothing
datumToSet _ = Nothing

-- lambda expression
datumToLambdaExact :: Datum -> Maybe Expression
datumToLambdaExact (List (Symbol "lambda" : List params : body)) =
  case checkListParam params of
    Just params' -> case datumsToExprs body of
      Just body' -> case nonEmpty body' of
        Just body'' -> Just $ Lambda (Exact params') body''
        Nothing -> Nothing
      _ -> Nothing
    _ -> Nothing
datumToLambdaExact (List (Symbol "lambda" : Symbol arg : body)) =
  case datumsToExprs body of
    Just body' -> case nonEmpty body' of
      Just body'' -> Just $ Lambda (Variadic [] arg) body''
      _ -> error "here"
    _ -> error "here"
datumToLambdaExact _ = Nothing

datumToLambdaVariadic :: Datum -> Maybe Expression
datumToLambdaVariadic
  (List (Symbol "lambda" : DottedList params (Symbol v) : body)) =
    case checkListParam params of
      Just params' -> case datumsToExprs body of
        Just body' -> case nonEmpty body' of
          Just body'' -> Just $ Lambda (Variadic params' v) body''
          Nothing -> Nothing
        _ -> Nothing
      _ -> Nothing
datumToLambdaVariadic _ = Nothing

datumToLambda :: Datum -> Maybe Expression
datumToLambda d =
  datumToLambdaExact d
    <|> datumToLambdaVariadic d
    <|> Nothing

-- let expression (convert in lambda)
-- (let ((var val) ...) body) -> ((lambda (var ...) body) val ...)
extractKeyValue :: Datum -> Maybe (Datum, Datum)
extractKeyValue (List [a, b]) = Just (a, b)
extractKeyValue _ = Nothing

extractKeysValues :: [Datum] -> Maybe ([Datum], [Datum])
extractKeysValues [] = Just ([], [])
extractKeysValues (x : xs) = case extractKeyValue x of
  Just (a, b) -> case extractKeysValues xs of
    Just (as, bs) -> Just (a : as, b : bs)
    _ -> Nothing
  _ -> Nothing

datumToLet :: Datum -> Maybe Expression
datumToLet (List (Symbol "let" : List vars : body)) = case keysValues of
  Just (keys, values) -> datumToExpr $ List (lambda : values)
    where
      lambda = List (Symbol "lambda" : List keys : body)
  _ -> Nothing
  where
    keysValues = extractKeysValues vars
datumToLet _ = Nothing

-- var expressions
datumToVar :: Datum -> Maybe Expression
datumToVar (Symbol s) = Just $ Var s
datumToVar _ = Nothing

-- load expression
datumToLoad :: Datum -> Maybe Expression
datumToLoad (List [Symbol "load", path]) =
  case datumToExpr path of
    Just path' -> Just $ Load path'
    _ -> Nothing
datumToLoad _ = Nothing

-- application expression
datumToApplication :: Datum -> Maybe Expression
datumToApplication (List (a : xs)) =
  case (datumToExpr a, datumsToExprs xs) of
    (Just a', Just xs') -> Just $ Application a' xs'
    _ -> Nothing
datumToApplication _ = Nothing

-- expressions
datumToExpr :: Datum -> Maybe Expression
datumToExpr d =
  (Quote <$> datumToQuote d)
    <|> (Const <$> datumToConst d)
    <|> datumToBeginExpr d
    <|> datumToIf d
    <|> datumToLet d
    <|> datumToLoad d
    <|> datumToCond d
    <|> datumToSet d
    <|> datumToLambda d
    <|> datumToApplication d
    <|> datumToVar d

datumsToExprs :: [Datum] -> Maybe [Expression]
datumsToExprs [] = Just []
datumsToExprs (x : xs) = case datumToExpr x of
  Just x -> case datumsToExprs xs of
    Just xs -> Just $ x : xs
    _ -> Nothing
  _ -> Nothing

-- definition
datumToDef :: Datum -> Maybe Definition
datumToDef a = datumToDefine a <|> datumToBeginDef a

datumsToDefs :: [Datum] -> Maybe [Definition]
datumsToDefs [] = Just []
datumsToDefs (x : xs) = case datumToDef x of
  Just x -> case datumsToDefs xs of
    Just xs -> Just $ x : xs
    _ -> Nothing
  _ -> Nothing

-- form
datumToForm :: Datum -> Maybe Form
datumToForm datum =
  Definition <$> datumToDef datum <|> Expression <$> datumToExpr datum

datumsToForms :: [Datum] -> Maybe [Form]
datumsToForms [] = Just []
datumsToForms [datum] = case datumToForm datum of
  Just form -> Just [form]
  _ -> Nothing
datumsToForms (datum : datums) = case datumToForm datum of
  Just form -> case datumsToForms datums of
    Just forms -> Just $ form : forms
    _ -> Nothing
  _ -> Nothing
