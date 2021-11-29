module Option
  ( parseArgs,
    helpMessage,
    Options (..),
  )
where

import System.Console.GetOpt
  ( ArgDescr (NoArg, ReqArg),
    ArgOrder (Permute),
    OptDescr (..),
    getOpt,
    usageInfo,
  )
import System.Environment (getArgs)

data Options = Options
  { optHelp :: Bool,
    optIsQuiet :: Bool,
    optInteractive :: Bool,
    optLibraries :: [FilePath],
    optFiles :: [FilePath]
  }
  deriving (Show)

defaultOptions :: Options
defaultOptions =
  Options
    { optHelp = False,
      optIsQuiet = False,
      optInteractive = False,
      optLibraries = ["./lang/base.scm"],
      optFiles = []
    }

-- options
optionHelp :: OptDescr (Options -> Options)
optionHelp =
  Option
    ['h']
    ["help"]
    (NoArg (\opt -> opt {optHelp = True}))
    "Show this help and exit"

optionQuiet :: OptDescr (Options -> Options)
optionQuiet =
  Option
    ['q']
    ["quiet"]
    (NoArg (\opt -> opt {optIsQuiet = True}))
    "Suppress prompt"

optionInteractive :: OptDescr (Options -> Options)
optionInteractive =
  Option
    ['i']
    ["interactive"]
    (NoArg (\opt -> opt {optInteractive = True}))
    "Enable interactive mode"

optionLibrary :: OptDescr (Options -> Options)
optionLibrary =
  Option
    ['l']
    ["library"]
    ( ReqArg
        (\path opt -> opt {optLibraries = optLibraries opt ++ [path]})
        "FILE/DIR PATH"
    )
    "Add a library path"

options :: [OptDescr (Options -> Options)]
options =
  [ optionHelp,
    optionQuiet,
    optionInteractive,
    optionLibrary
  ]

helpMessage :: String
helpMessage = usageInfo header options
  where
    header = "Usage: ./hal [FILE]... [OPTION]..."

-- parser
parseArgs :: IO (Either String Options)
parseArgs = do
  args <- getArgs
  case getOpt Permute options args of
    (opts, files, []) -> pure $ Right $ opts' {optFiles = files}
      where
        opts' = foldl (flip id) defaultOptions opts
    (_, _, errs) -> pure $ Left (concat errs ++ tryHelp)
  where
    tryHelp = "Try './hal --help' for more information."
