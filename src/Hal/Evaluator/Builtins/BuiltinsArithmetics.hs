module Hal.Evaluator.Builtins.BuiltinsArithmetics
  ( builtinsArithmetics,
  )
where

import Data.Fixed (mod')
import Hal.Evaluator.Environment (EnvPrototype)
import Hal.Evaluator.LispVal (LispVal (..), NativeProcedure, isAtom)
import Hal.Exception (throwException)

type Env = EnvPrototype LispVal

builtinsArithmetics :: String -> Maybe NativeProcedure
builtinsArithmetics "+" = Just arithmeticAdd
builtinsArithmetics "-" = Just arithmeticSub
builtinsArithmetics "*" = Just arithmeticMul
builtinsArithmetics "div" = Just arithmeticDiv
builtinsArithmetics "mod" = Just arithmeticMod
builtinsArithmetics _ = Nothing

arithmeticAdd :: NativeProcedure
arithmeticAdd [] env = pure (Number 0, env)
arithmeticAdd [Number a] env = pure (Number a, env)
arithmeticAdd [Float a] env = pure (Float a, env)
arithmeticAdd (Number a : xs) env = do
  (res, env') <- arithmeticAdd xs env
  case res of
    (Number b) -> pure (Number (a + b), env')
    (Float b) -> pure (Float (fromIntegral a + b), env')
    _ -> throwException "+: not supported"
arithmeticAdd (Float a : xs) env = do
  (res, env') <- arithmeticAdd xs env
  case res of
    (Number b) -> pure (Float (a + fromIntegral b), env')
    (Float b) -> pure (Float (a + b), env')
    _ -> throwException "+: not supported"
arithmeticAdd _ _ = throwException "+: invalid arguments"

arithmeticSub :: NativeProcedure
arithmeticSub [Number a] env = pure (Number (- a), env)
arithmeticSub [Float a] env = pure (Float (- a), env)
arithmeticSub (Number a : xs) env = do
  (res, env') <- arithmeticAdd xs env
  case res of
    (Number b) -> pure (Number (a - b), env')
    (Float b) -> pure (Float (fromIntegral a - b), env')
    _ -> throwException "-: not supported"
arithmeticSub (Float a : xs) env = do
  (res, env') <- arithmeticAdd xs env
  case res of
    (Number b) -> pure (Float (a - fromIntegral b), env')
    (Float b) -> pure (Float (a - b), env')
    _ -> throwException "-: not supported"
arithmeticSub _ _ = throwException "-: invalid arguments"

arithmeticMul :: NativeProcedure
arithmeticMul [] env = pure (Number 1, env)
arithmeticMul [Number a] env = pure (Number a, env)
arithmeticMul [Float a] env = pure (Float a, env)
arithmeticMul (Number a : xs) env = do
  (res, env') <- arithmeticMul xs env
  case res of
    (Number b) -> pure (Number (a * b), env')
    (Float b) -> pure (Float (fromIntegral a * b), env')
    _ -> throwException "*: not supported"
arithmeticMul (Float a : xs) env = do
  (res, env') <- arithmeticMul xs env
  case res of
    (Number b) -> pure (Float (a * fromIntegral b), env')
    (Float b) -> pure (Float (a * b), env')
    _ -> throwException "*: not supported"
arithmeticMul _ _ = throwException "*: invalid arguments"

arithmeticDiv :: NativeProcedure
arithmeticDiv [_, Number 0] _ = throwException "div: undefined for 0"
arithmeticDiv [_, Float 0] _ = throwException "div: undefined for 0"
arithmeticDiv [Number a, Number b] env
  | b < 0 = pure (Number roundToInt, env)
  | otherwise = pure (Number (a `div` b), env)
  where
    res = fromIntegral a / fromIntegral b :: Double
    roundToInt = fromIntegral $ round res
arithmeticDiv [Float a, Float b] env = pure (Float (a / b), env)
arithmeticDiv [Number a, Float b] env =
  pure (Float (fromIntegral a / b), env)
arithmeticDiv [Float a, Number b] env =
  pure (Float (a / fromIntegral b), env)
arithmeticDiv _ _ = throwException "div: invalid arguments"

arithmeticMod :: NativeProcedure
arithmeticMod [_, Number 0] _ = throwException "mod: undefined for 0"
arithmeticMod [_, Float 0] _ = throwException "mod: undefined for 0"
arithmeticMod [Number a, Number b] env = pure (Number (a `mod` b), env)
arithmeticMod [Float a, Float b] env = pure (Float (a `mod'` b), env)
arithmeticMod [Number a, Float b] env =
  pure (Float (fromIntegral a `mod'` b), env)
arithmeticMod [Float a, Number b] env =
  pure (Float (a `mod'` fromIntegral b), env)
arithmeticMod _ _ = throwException "mod: invalid arguments"
