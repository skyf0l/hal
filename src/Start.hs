module Start
  ( hal,
  )
where

import Hal.Evaluator.Environment (emptyEnv)
import Hal.Interpreter (interpretFiles)
import Hal.Repl (runRepl)
import Option
  ( Options (..),
    helpMessage,
    parseArgs,
  )
import System.Exit
  ( ExitCode (ExitFailure),
    exitWith,
  )

-- main with parsed options
runHal :: Options -> IO ()
-- print help
runHal Options {optHelp = True} =
  putStr helpMessage
-- force interactive: libraries -> files -> repl
runHal
  Options
    { optIsQuiet = isQuiet,
      optInteractive = True,
      optFiles = files,
      optLibraries = libraries
    } = do
    (_, env) <- interpretFiles False True emptyEnv libraries
    (_, env') <- interpretFiles False True env files
    runRepl isQuiet env'
-- no files: libraries -> repl
runHal
  Options
    { optIsQuiet = isQuiet,
      optFiles = [],
      optLibraries = libraries
    } = do
    (_, env) <- interpretFiles False True emptyEnv libraries
    runRepl isQuiet env
-- files: libraries -> files
runHal
  opts@Options
    { optFiles = files,
      optLibraries = libraries
    } = do
    (_, env) <- interpretFiles False True emptyEnv libraries
    interpretFiles True True env files
    pure ()

-- main
hal :: IO ()
hal = do
  options <- parseArgs
  case options of
    Right options' -> runHal options'
    Left errorMsg -> putStrLn errorMsg >> exitWith (ExitFailure 84)
