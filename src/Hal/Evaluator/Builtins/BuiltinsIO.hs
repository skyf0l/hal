module Hal.Evaluator.Builtins.BuiltinsIO
  ( builtinsIO,
  )
where

import Hal.Evaluator.Environment (EnvPrototype)
import Hal.Evaluator.LispVal (LispVal (..), NativeProcedure, isAtom)
import Hal.Exception (throwException)

type Env = EnvPrototype LispVal

builtinsIO :: String -> Maybe NativeProcedure
builtinsIO "display" = Just ioDisplay
builtinsIO "newline" = Just ioNewline
builtinsIO _ = Nothing

ioDisplay :: NativeProcedure
ioDisplay [Void] env = putStr "#<void>" >> pure (Void, env)
ioDisplay [String s] env = putStr s >> pure (Void, env)
ioDisplay [x] env = putStr (show x) >> pure (Void, env)
ioDisplay _ _ = throwException "display: invalid arguments"

ioNewline :: NativeProcedure
ioNewline [] env = putStrLn "" >> pure (Void, env)
ioNewline _ _ = throwException "newline: invalid arguments"