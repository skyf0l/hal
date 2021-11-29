module Hal.Evaluator.Builtins.BuiltinsPredicates
  ( builtinPredicates,
  )
where

import Data.Fixed (mod')
import Hal.Evaluator.Environment (EnvPrototype)
import Hal.Evaluator.LispVal (LispVal (..), NativeProcedure, isAtom)
import Hal.Exception (throwException)

type Env = EnvPrototype LispVal

builtinPredicates :: String -> Maybe NativeProcedure
builtinPredicates "eq?" = Just $ predicate builtinEq
builtinPredicates "atom?" = Just $ predicate builtinAtom
builtinPredicates "void?" = Just $ predicate builtinIsVoid
builtinPredicates "boolean?" = Just $ predicate builtinIsBool
builtinPredicates "symbol?" = Just $ predicate builtinIsSymbol
builtinPredicates "char?" = Just $ predicate builtinIsChar
builtinPredicates "string?" = Just $ predicate builtinIsString
builtinPredicates "pair?" = Just $ predicate builtinIsPair
builtinPredicates "null?" = Just $ predicate builtinIsNull
builtinPredicates "number?" = Just $ predicate builtinIsNumber
builtinPredicates "real?" = Just $ predicate builtinIsReal
builtinPredicates "rational?" = Just $ predicate builtinIsRational
builtinPredicates "complex?" = Just $ predicate builtinIsComplex
builtinPredicates "exact?" = Just $ predicate builtinIsExact
builtinPredicates "inexact?" = Just $ predicate builtinIsInexact
builtinPredicates "integer?" = Just $ predicate builtinIsInteger
builtinPredicates "finite?" = Just $ predicate builtinIsFinite
builtinPredicates "infinite?" = Just $ predicate builtinIsInfinite
builtinPredicates "nan?" = Just $ predicate builtinIsNaN
builtinPredicates _ = Nothing

predicate :: ([LispVal] -> IO Bool) -> NativeProcedure
predicate p args env = do
  res <- p args
  pure (Bool res, env)

builtinEq :: [LispVal] -> IO Bool
builtinEq [a, b] = pure $ a == b
builtinEq _ = throwException "eq: invalid arguments"

builtinAtom :: [LispVal] -> IO Bool
builtinAtom [a] = pure $ isAtom a
builtinAtom _ = throwException "atom: invalid arguments"

-- basic types
builtinIsVoid :: [LispVal] -> IO Bool
builtinIsVoid [Void] = pure True
builtinIsVoid [_] = pure False
builtinIsVoid _ = throwException "void?: invalid arguments"

builtinIsBool :: [LispVal] -> IO Bool
builtinIsBool [Bool _] = pure True
builtinIsBool [_] = pure False
builtinIsBool _ = throwException "boolean?: invalid arguments"

builtinIsSymbol :: [LispVal] -> IO Bool
builtinIsSymbol [Symbol _] = pure True
builtinIsSymbol [_] = pure False
builtinIsSymbol _ = throwException "symbol?: invalid arguments"

builtinIsChar :: [LispVal] -> IO Bool
builtinIsChar [Char _] = pure True
builtinIsChar [_] = pure False
builtinIsChar _ = throwException "char?: invalid arguments"

builtinIsString :: [LispVal] -> IO Bool
builtinIsString [String _] = pure True
builtinIsString [_] = pure False
builtinIsString _ = throwException "string?: invalid arguments"

builtinIsPair :: [LispVal] -> IO Bool
builtinIsPair [Pair _ _] = pure True
builtinIsPair [_] = pure False
builtinIsPair _ = throwException "pair?: invalid arguments"

builtinIsNull :: [LispVal] -> IO Bool
builtinIsNull [Empty] = pure True
builtinIsNull [_] = pure False
builtinIsNull _ = throwException "null?: invalid arguments"

-- number
builtinIsNumber :: [LispVal] -> IO Bool
builtinIsNumber [Number _] = pure True
builtinIsNumber [Float _] = pure True
builtinIsNumber [_] = pure False
builtinIsNumber _ = throwException "number?: invalid arguments"

builtinIsReal :: [LispVal] -> IO Bool
builtinIsReal [Number _] = pure True
builtinIsReal [Float _] = pure True
builtinIsReal [_] = pure False
builtinIsReal _ = throwException "real?: invalid arguments"

builtinIsRational :: [LispVal] -> IO Bool
builtinIsRational [Number _] = pure True
builtinIsRational [Float _] = pure True
builtinIsRational [_] = pure False
builtinIsRational _ = throwException "rational?: invalid arguments"

builtinIsComplex :: [LispVal] -> IO Bool
builtinIsComplex [Number _] = pure True
builtinIsComplex [Float _] = pure True
builtinIsComplex [_] = pure False
builtinIsComplex _ = throwException "complex?: invalid arguments"

builtinIsExact :: [LispVal] -> IO Bool
builtinIsExact [Number _] = pure True
builtinIsExact [_] = pure False
builtinIsExact _ = throwException "exact?: invalid arguments"

builtinIsInexact :: [LispVal] -> IO Bool
builtinIsInexact [Float _] = pure True
builtinIsInexact [_] = pure False
builtinIsInexact _ = throwException "inexact?: invalid arguments"

builtinIsInteger :: [LispVal] -> IO Bool
builtinIsInteger [Number _] = pure True
builtinIsInteger [_] = pure False
builtinIsInteger _ = throwException "integer?: invalid arguments"

builtinIsFinite :: [LispVal] -> IO Bool
builtinIsFinite [Number _] = pure True
builtinIsFinite [Float _] = pure True
builtinIsFinite [_] = pure False
builtinIsFinite _ = throwException "finite?: invalid arguments"

builtinIsInfinite :: [LispVal] -> IO Bool
builtinIsInfinite [_] = pure False
builtinIsInfinite _ = throwException "infinite?: invalid arguments"

builtinIsNaN :: [LispVal] -> IO Bool
builtinIsNaN [_] = pure False
builtinIsNaN _ = throwException "nan?: invalid arguments"