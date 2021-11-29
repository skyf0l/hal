module Hal.Evaluator.Builtins.Builtins
  ( builtinGet,
  )
where

import Control.Applicative (Alternative ((<|>)))
import Hal.Evaluator.Builtins.BuiltinsArithmetics (builtinsArithmetics)
import Hal.Evaluator.Builtins.BuiltinsComparisons (builtinsComparisons)
import Hal.Evaluator.Builtins.BuiltinsIO (builtinsIO)
import Hal.Evaluator.Builtins.BuiltinsLists (builtinsLists)
import Hal.Evaluator.Builtins.BuiltinsPredicates (builtinPredicates)
import Hal.Evaluator.Builtins.BuiltinsStrings (builtinsStrings)
import Hal.Evaluator.Environment (EnvPrototype)
import Hal.Evaluator.LispVal (LispVal (..), NativeProcedure, Procedure (..), isAtom)
import Hal.Exception (throwException)
import Hal.Parser.Parser (parseStringToDatums)

type Env = EnvPrototype LispVal

builtinGet :: String -> Maybe Procedure
builtinGet name =
  NativeP
    <$> ( builtins name
            <|> builtinsArithmetics name
            <|> builtinPredicates name
            <|> builtinsComparisons name
            <|> builtinsStrings name
            <|> builtinsIO name
            <|> builtinsLists name
        )

builtins :: String -> Maybe NativeProcedure
builtins "void" = Just builtinVoid
builtins "car" = Just builtinCar
builtins "cdr" = Just builtinCdr
builtins "cons" = Just builtinPair
builtins _ = Nothing

builtinVoid :: NativeProcedure
builtinVoid [] env = pure (Void, env)
builtinVoid _ env = throwException "void: invalid arguments"

builtinCar :: NativeProcedure
builtinCar [Pair a _] env = pure (a, env)
builtinCar [_] env = throwException "car: expected a pair"
builtinCar _ _ = throwException "car: invalid arguments"

builtinCdr :: NativeProcedure
builtinCdr [Pair _ b] env = pure (b, env)
builtinCdr [_] env = throwException "cdr: expected a pair"
builtinCdr _ _ = throwException "cdr: invalid arguments"

builtinPair :: NativeProcedure
builtinPair [a, b] env = pure (Pair a b, env)
builtinPair _ _ = throwException "cons: invalid arguments"
