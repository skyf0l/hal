module Hal.Repl
  ( runRepl,
  )
where

import Hal.Evaluator.DatumToForm (datumsToForms)
import Hal.Evaluator.Environment
  ( EnvPrototype (EnvPrototype),
    deleteScope,
    newScope,
  )
import Hal.Evaluator.Evaluator (Env, LispVal (Void), emptyEnv)
import Hal.Interpreter (interpretString)
import Hal.Parser.Parser (parseStringToDatums)
import System.Exit (exitSuccess)
import System.IO (hFlush, hPutStrLn, isEOF, stderr, stdout)

-- messages
replHelpMsg :: String
replHelpMsg =
  "Available commands:\n"
    ++ "  :h - show this help\n"
    ++ "  :q - quit\n"
    ++ "  :c - clear the environment\n"
    ++ "  :s - show the environment\n"
    ++ "  :e - enter in new scope\n"
    ++ "  :r - exit current scope\n\n"
    ++ "Debug commands:\n"
    ++ "  :d - show Datum of parsed expression (not update the environment)\n"
    ++ "  :f - show Form of parsed Datum (not update the environment)"

replPrompt :: String
replPrompt = "> "

replHeader :: String
replHeader =
  "HAL - Lisp REPL\n"
    ++ "Type :h to see help\n"
    ++ "Type :q to quit\n"

-- read
replRead' :: Bool -> IO String
replRead' False = do
  end <- isEOF
  case end of
    False -> getLine
    True -> putStrLn "" >> hFlush stdout >> exitSuccess
replRead' isTrue = do
  end <- isEOF
  case end of
    False -> getLine
    True -> exitSuccess

replRead :: Bool -> IO String
replRead isQuiet@False =
  putStr replPrompt
    >> hFlush stdout
    >> replRead' isQuiet
replRead isQuiet@True = replRead' isQuiet

-- eval cmd
replUnknownCmdCommand :: Env -> String -> IO Env
replUnknownCmdCommand env cmd =
  hPutStrLn stderr "Unknown command"
    >> hPutStrLn stderr "Type :h to see help"
    >> pure env

replEvalCmd :: Env -> String -> IO Env
replEvalCmd env "q" = exitSuccess
replEvalCmd env "h" = putStrLn replHelpMsg >> hFlush stdout >> pure env
replEvalCmd env "c" = pure emptyEnv
replEvalCmd env@(EnvPrototype e) "s" = print e >> pure env
replEvalCmd env "e" = pure $ newScope env
replEvalCmd env "r" = pure $ deleteScope env
replEvalCmd env ('d' : expr) =
  case parseStringToDatums expr of
    Just datums -> print datums >> pure env
    Nothing -> pure env
replEvalCmd env ('f' : expr) =
  case parseStringToDatums expr of
    Just datums -> case datumsToForms datums of
      Just forms -> print forms >> pure env
      Nothing -> pure env
    Nothing -> pure env
replEvalCmd env expr = replUnknownCmdCommand env expr

-- eval
replEval :: Env -> String -> IO (LispVal, Env)
replEval env "" = pure (Void, env)
replEval env (':' : cmd) = do
  env' <- replEvalCmd env cmd
  pure (Void, env')
replEval env line = interpretString False env line

-- print
replPrint :: LispVal -> IO ()
replPrint result = case result of
  Void -> pure ()
  res' -> print res'

-- loop
repl :: Bool -> Env -> IO ()
repl isQuiet env = do
  line <- replRead isQuiet
  (lispVal, env') <- replEval env line
  replPrint lispVal
  repl isQuiet env'

-- Open repl with isQuiet bool
-- if isQuiet is true, not display repl header and prompt
runRepl :: Bool -> Env -> IO ()
runRepl isQuiet@False env = putStrLn replHeader >> repl isQuiet env
runRepl isQuiet@True env = repl isQuiet env
