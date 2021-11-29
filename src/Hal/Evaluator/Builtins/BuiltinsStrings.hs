module Hal.Evaluator.Builtins.BuiltinsStrings
  ( builtinsStrings,
  )
where

import Hal.Evaluator.Environment (EnvPrototype)
import Hal.Evaluator.LispVal (LispVal (..), NativeProcedure)
import Hal.Exception (throwException)

type Env = EnvPrototype LispVal

builtinsStrings :: String -> Maybe NativeProcedure
builtinsStrings "string=?" = Just compareStringEq
builtinsStrings "string<?" = Just compareStringLt
builtinsStrings "string>?" = Just compareStringGt
builtinsStrings "string<=?" = Just compareStringLe
builtinsStrings "string>=?" = Just compareStringGe
builtinsStrings _ = Nothing

compareStringEq :: NativeProcedure
compareStringEq [String _] env = pure (Bool True, env)
compareStringEq [String a, String b] env = case compare a b of
  EQ -> pure (Bool True, env)
  _ -> pure (Bool False, env)
compareStringEq (String a : String b : xs) env = do
  (res, env') <- compareStringEq [String a, String b] env
  case res of
    (Bool True) -> compareStringEq (String b : xs) env'
    _ -> pure (res, env')
compareStringEq _ _ = throwException "string=? invalid arguments"

compareStringLt :: NativeProcedure
compareStringLt [String _] env = pure (Bool True, env)
compareStringLt [String a, String b] env = case compare a b of
  LT -> pure (Bool True, env)
  _ -> pure (Bool False, env)
compareStringLt (String a : String b : xs) env = do
  (res, env') <- compareStringLt [String a, String b] env
  case res of
    (Bool True) -> compareStringLt (String b : xs) env'
    _ -> pure (res, env')
compareStringLt _ _ = throwException "string<? invalid arguments"

compareStringGt :: NativeProcedure
compareStringGt [String _] env = pure (Bool True, env)
compareStringGt [String a, String b] env = case compare a b of
  GT -> pure (Bool True, env)
  _ -> pure (Bool False, env)
compareStringGt (String a : String b : xs) env = do
  (res, env') <- compareStringGt [String a, String b] env
  case res of
    (Bool True) -> compareStringGt (String b : xs) env'
    _ -> pure (res, env')
compareStringGt _ _ = throwException "string>? invalid arguments"

compareStringLe :: NativeProcedure
compareStringLe [String _] env = pure (Bool True, env)
compareStringLe [String a, String b] env = case compare a b of
  LT -> pure (Bool True, env)
  EQ -> pure (Bool True, env)
  _ -> pure (Bool False, env)
compareStringLe (String a : String b : xs) env = do
  (res, env') <- compareStringLe [String a, String b] env
  case res of
    (Bool True) -> compareStringLe (String b : xs) env'
    _ -> pure (res, env')
compareStringLe _ _ = throwException "string<=? invalid arguments"

compareStringGe :: NativeProcedure
compareStringGe [String _] env = pure (Bool True, env)
compareStringGe [String a, String b] env = case compare a b of
  GT -> pure (Bool True, env)
  EQ -> pure (Bool True, env)
  _ -> pure (Bool False, env)
compareStringGe (String a : String b : xs) env = do
  (res, env') <- compareStringGe [String a, String b] env
  case res of
    (Bool True) -> compareStringGe (String b : xs) env'
    _ -> pure (res, env')
compareStringGe _ _ = throwException "string>=? invalid arguments"