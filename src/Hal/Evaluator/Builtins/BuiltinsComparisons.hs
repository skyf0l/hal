module Hal.Evaluator.Builtins.BuiltinsComparisons
  ( builtinsComparisons,
  )
where

import Hal.Evaluator.Environment (EnvPrototype)
import Hal.Evaluator.LispVal (LispVal (..), NativeProcedure)
import Hal.Exception (throwException)

type Env = EnvPrototype LispVal

builtinsComparisons :: String -> Maybe NativeProcedure
builtinsComparisons "max" = Just compareMax
builtinsComparisons "min" = Just compareMin
builtinsComparisons ">" = Just $ compareOp (>) ">"
builtinsComparisons "<" = Just $ compareOp (<) "<"
builtinsComparisons ">=" = Just $ compareOp (>=) ">="
builtinsComparisons "<=" = Just $ compareOp (<=) "<="
builtinsComparisons "==" = Just $ compareOp (==) "=="
builtinsComparisons "/=" = Just $ compareOp (/=) "/="
builtinsComparisons "or" = Just compareLogicalOr
builtinsComparisons "and" = Just compareLogicalAnd
builtinsComparisons "not" = Just compareLogicalNot
builtinsComparisons _ = Nothing

compareMax :: NativeProcedure
compareMax [Number a] env = pure (Number a, env)
compareMax [Float a] env = pure (Float a, env)
compareMax [Number a, Number b] env = pure (Number $ max a b, env)
compareMax [Number a, Float b] env =
  pure (Float $ max (fromIntegral a) b, env)
compareMax [Float a, Number b] env =
  pure (Float $ max a (fromIntegral b), env)
compareMax [Float a, Float b] env = pure (Float $ max a b, env)
compareMax (Number a : xs) env = do
  (b, env') <- compareMax xs env
  compareMax [Number a, b] env'
compareMax (Float a : xs) env = do
  (b, env') <- compareMax xs env
  compareMax [Float a, b] env'
compareMax _ _ = throwException "max: invalid arguments"

compareMin :: NativeProcedure
compareMin [Number a] env = pure (Number a, env)
compareMin [Float a] env = pure (Float a, env)
compareMin [Number a, Number b] env = pure (Number $ min a b, env)
compareMin [Number a, Float b] env =
  pure (Float $ min (fromIntegral a) b, env)
compareMin [Float a, Number b] env =
  pure (Float $ min a (fromIntegral b), env)
compareMin [Float a, Float b] env = pure (Float $ min a b, env)
compareMin (Number a : xs) env = do
  (b, env') <- compareMin xs env
  compareMin [Number a, b] env'
compareMin (Float a : xs) env = do
  (b, env') <- compareMin xs env
  compareMin [Float a, b] env'
compareMin _ _ = throwException "min: invalid arguments"

compareOp :: (Float -> Float -> Bool) -> String -> NativeProcedure
compareOp _ _ [Number a] env = pure (Bool True, env)
compareOp _ _ [Float a] env = pure (Bool True, env)
compareOp op _ [Float a, Float b] env = pure (Bool $ a < b, env)
compareOp op _ [Number a, Float b] env = pure (Bool $ a' < b, env)
  where
    a' = fromIntegral a
compareOp op _ [Float a, Number b] env = pure (Bool $ a < b', env)
  where
    b' = fromIntegral b
compareOp op _ [Number a, Number b] env = pure (Bool $ op a' b', env)
  where
    a' = fromIntegral a
    b' = fromIntegral b
compareOp op name (Number a : Number b : xs) env = do
  (res, env') <- compareOp op name [Number a, Number b] env
  case res of
    Bool True -> compareOp op name (Number b : xs) env'
    Bool False -> pure (Bool False, env')
    _ -> throwException $ name ++ ": not supported"
compareOp _ name _ _ = throwException $ name ++ ": invalid arguments"

compareLogicalOr :: NativeProcedure
compareLogicalOr [] env = pure (Bool False, env)
compareLogicalOr (Bool False : xs) env = compareLogicalOr xs env
compareLogicalOr (x : xs) env = pure (x, env)

compareLogicalAnd :: NativeProcedure
compareLogicalAnd [] env = pure (Bool True, env)
compareLogicalAnd (Bool False : _) env = pure (Bool False, env)
compareLogicalAnd [x] env = pure (x, env)
compareLogicalAnd (x : xs) env = compareLogicalAnd xs env

compareLogicalNot :: NativeProcedure
compareLogicalNot [Bool False] env = pure (Bool True, env)
compareLogicalNot [_] env = pure (Bool False, env)
compareLogicalNot _ _ = throwException "not: invalid arguments"