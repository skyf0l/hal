module Hal.Evaluator.Builtins.BuiltinsLists
  ( builtinsLists,
  )
where

import Hal.Evaluator.Environment (EnvPrototype)
import Hal.Evaluator.LispVal (LispVal (..), NativeProcedure)
import Hal.Exception (throwException)

type Env = EnvPrototype LispVal

builtinsLists :: String -> Maybe NativeProcedure
builtinsLists "append" = Just listAppend
builtinsLists "reverse" = Just listReverse
builtinsLists _ = Nothing

listAppend :: NativeProcedure
listAppend [] env = pure (Empty, env)
listAppend [Empty] env = pure (Empty, env)
listAppend [list] env = pure (list, env)
listAppend (Empty : lists) env = listAppend lists env
listAppend (Pair x Empty : lists) env = do
  (lists', env') <- listAppend lists env
  pure (Pair x lists', env')
listAppend (Pair x xs : lists) env = do
  (lists', env') <- listAppend (xs : lists) env
  pure (Pair x lists', env')
listAppend _ _ = throwException "append: invalid arguments"

listReverse :: NativeProcedure
listReverse [Empty] env = pure (Empty, env)
listReverse [Pair x Empty] env = pure ((Pair x Empty), env)
listReverse [Pair x xs] env = do
  (xs', env') <- listReverse [xs] env
  listAppend [xs', Pair x Empty] env'
listReverse _ _ = throwException "reverse: invalid arguments"
