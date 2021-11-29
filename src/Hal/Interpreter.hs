module Hal.Interpreter
  ( interpretFile,
    interpretFiles,
    interpretString,
  )
where

import Control.Exception (SomeException (SomeException), try)
import Hal.Evaluator.Evaluator (Env, evalDatums)
import Hal.Evaluator.LispVal (LispVal (Void))
import Hal.Exception (HalException, throwException)
import Hal.Parser.Parser (parseStringToDatums)
import System.Exit (ExitCode (ExitFailure), exitSuccess, exitWith)
import System.IO (hFlush, hPrint, openFile, stderr)

exitFail :: IO a
exitFail = exitWith $ ExitFailure 84

parseAndEval :: String -> Env -> IO (LispVal, Env)
parseAndEval expr env = case parseStringToDatums expr of
  Nothing -> throwException "Parsing error"
  Just datums -> evalDatums datums env

interpretString :: Bool -> Env -> String -> IO (LispVal, Env)
interpretString exitOnError env expr = do
  res <- try (parseAndEval expr env) :: IO (Either HalException (LispVal, Env))
  case res of
    Left err ->
      hPrint stderr err >> hFlush stderr
        >> if exitOnError then exitFail else pure (Void, env)
    Right res' -> pure res'

readFileContent :: FilePath -> IO String
readFileContent filePath = do
  file <- try (readFile filePath) :: IO (Either SomeException String)
  case file of
    Left err -> hPrint stderr err >> hFlush stderr >> exitFail
    Right content -> pure content

interpretFile :: Bool -> Env -> FilePath -> IO (LispVal, Env)
interpretFile exitOnError env filePath = do
  content <- readFileContent filePath
  interpretString exitOnError env content

displayResult :: Bool -> LispVal -> IO ()
displayResult False _ = pure ()
displayResult True lispVal = case lispVal of
  Void -> pure ()
  res' -> print res'

interpretFiles :: Bool -> Bool -> Env -> [FilePath] -> IO (LispVal, Env)
interpretFiles _ _ env [] = pure (Void, env)
interpretFiles display exitOnError env [filePath] = do
  res@(lispVal, env') <- interpretFile exitOnError env filePath
  displayResult display lispVal
  pure res
interpretFiles display exitOnError env (filePath : filePaths) = do
  (_, env') <- interpretFile exitOnError env filePath
  interpretFiles display exitOnError env' filePaths
