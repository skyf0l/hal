module Hal.Evaluator.Environment
  ( EnvPrototype (..),
    emptyEnv,
    envSet,
    envGet,
    envDef,
    newScope,
    deleteScope,
  )
where

import Data.List.NonEmpty (NonEmpty (..))

newtype EnvPrototype a = EnvPrototype (NonEmpty [(String, a)])
  deriving (Show)

emptyEnv :: EnvPrototype a
emptyEnv = EnvPrototype ([] :| [])

replace :: Eq a => (a, b) -> (a, b) -> (a, b)
replace (key, val) (k, v) = case k == key of
  True -> (k, val)
  False -> (k, v)

-- define a new variable in the current scope
envDef :: EnvPrototype a -> String -> a -> EnvPrototype a
envDef (EnvPrototype (env :| envs)) key val = case lookup key env of
  Nothing -> EnvPrototype (((key, val) : env) :| envs)
  Just _ -> EnvPrototype $ map replace' env :| envs
    where
      replace' = replace (key, val)

-- if value is already in the environment, replace it in first scope it is found
-- otherwise, add it to the first scope
envSetInChilds :: EnvPrototype a -> String -> a -> Maybe (EnvPrototype a)
envSetInChilds (EnvPrototype (env :| [])) key val = case lookup key env of
  Nothing -> Nothing
  Just _ -> Just $ EnvPrototype $ map replace' env :| []
    where
      replace' = replace (key, val)
envSetInChilds (EnvPrototype (env :| (x : xs))) key val = case lookup key env of
  Nothing -> case envs of
    (Just (EnvPrototype (x' :| xs'))) -> Just $ EnvPrototype (env :| (x' : xs'))
    Nothing -> Nothing
    where
      envs = envSetInChilds (EnvPrototype (x :| xs)) key val
  Just _ -> Just $ EnvPrototype $ map replace' env :| (x : xs)
    where
      replace' = replace (key, val)

envSet :: EnvPrototype a -> String -> a -> EnvPrototype a
envSet (EnvPrototype (env :| xs)) key val = case envs of
  Just envs' -> envs'
  Nothing -> EnvPrototype $ ((key, val) : env) :| xs
  where
    envs = envSetInChilds (EnvPrototype (env :| xs)) key val

-- get first value satisfying the predicate in scopes
envGet :: EnvPrototype a -> String -> Maybe a
envGet (EnvPrototype (env :| [])) key = lookup key env
envGet (EnvPrototype (env :| (x : xs))) key = case lookup key env of
  Just v -> Just v
  Nothing -> envGet (EnvPrototype (x :| xs)) key

-- create new scope (append empty list to the start of the env)
newScope :: EnvPrototype a -> EnvPrototype a
newScope (EnvPrototype (env :| [])) = EnvPrototype ([] :| [env])
newScope (EnvPrototype (env :| envs)) = EnvPrototype ([] :| (env : envs))

-- delete scope
deleteScope :: EnvPrototype a -> EnvPrototype a
deleteScope (EnvPrototype (env :| [])) = EnvPrototype (env :| [])
deleteScope (EnvPrototype (_ :| (x : xs))) = EnvPrototype (x :| xs)