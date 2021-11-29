module Hal.Evaluator.Evaluator
  ( Env (..),
    LispVal (..),
    emptyEnv,
    evalDatums,
    evalForms,
  )
where

import Control.Exception (SomeException (SomeException), try)
import Data.List.NonEmpty (NonEmpty (..), fromList, toList)
import Hal.Evaluator.Builtins.Builtins (builtinGet)
import Hal.Evaluator.DatumToForm (datumsToForms)
import Hal.Evaluator.Environment
  ( EnvPrototype (..),
    deleteScope,
    emptyEnv,
    envDef,
    envGet,
    envSet,
    newScope,
  )
import Hal.Evaluator.Form
  ( Clause (..),
    Constant (..),
    Definition (..),
    Expression (..),
    Form (..),
    Formal (..),
  )
import Hal.Evaluator.LispVal
  ( LispVal (..),
    Procedure (..),
  )
import Hal.Exception (HalException (..), throwException)
import Hal.Parser.Datum (Datum (..))
import Hal.Parser.Parser (parseStringToDatums)

type Env = EnvPrototype LispVal

-- lambda expression
evalLambda :: Expression -> Env -> IO (LispVal, Env)
evalLambda (Lambda formal body) env = pure (Procedure procedure, env)
  where
    procedure = LambdaP formal body
evalLambda _ _ = throwException "Lambda: not supported"

-- cond expression
evalCond :: Expression -> Env -> IO (LispVal, Env)
evalCond (Cond clauses) env = evalClauses clauses env
evalCond _ _ = throwException "Cond: not supported"

evalClauses :: NonEmpty Clause -> Env -> IO (LispVal, Env)
evalClauses (Else exprs :| []) env = evalExprs exprs env
evalClauses (Clause p v :| cs) env = do
  (res, env') <- evalExpr p env
  case res of
    (Hal.Evaluator.LispVal.Bool False) -> evalClauses (fromList cs) env'
    _ -> evalExprs v env'
evalClauses _ _ = throwException "Cond: not supported"

-- if expression
evalIf :: Expression -> Env -> IO (LispVal, Env)
evalIf (If2 cond thenExpr) env = do
  (cond', env') <- evalExpr cond env
  case cond' of
    (Hal.Evaluator.LispVal.Bool False) -> pure (Void, env')
    _ -> evalExpr thenExpr env'
evalIf (If3 cond thenExpr elseExpr) env = do
  (cond', env') <- evalExpr cond env
  case cond' of
    (Hal.Evaluator.LispVal.Bool False) -> evalExpr elseExpr env'
    _ -> evalExpr thenExpr env'
evalIf _ _ = throwException "If: not supported"

-- quote expression
evalQuotedDatum :: Datum -> LispVal
evalQuotedDatum (Hal.Parser.Datum.Bool b) =
  Hal.Evaluator.LispVal.Bool b
evalQuotedDatum (Hal.Parser.Datum.Number n) =
  Hal.Evaluator.LispVal.Number n
evalQuotedDatum (Hal.Parser.Datum.Float f) =
  Hal.Evaluator.LispVal.Float f
evalQuotedDatum (Hal.Parser.Datum.String s) =
  Hal.Evaluator.LispVal.String s
evalQuotedDatum (Hal.Parser.Datum.Char s) =
  Hal.Evaluator.LispVal.Char s
evalQuotedDatum (Hal.Parser.Datum.Symbol s) =
  Hal.Evaluator.LispVal.Symbol s
evalQuotedDatum (Hal.Parser.Datum.DottedList a b) = evalQuotedDatums a b
evalQuotedDatum (Hal.Parser.Datum.List a) =
  foldr (Pair . evalQuotedDatum) Hal.Evaluator.LispVal.Empty a

evalQuotedDatums :: [Datum] -> Datum -> LispVal
evalQuotedDatums [] d = Pair Empty $ evalQuotedDatum d
evalQuotedDatums [a] b = Pair (evalQuotedDatum a) (evalQuotedDatum b)
evalQuotedDatums (x : xs) b = Pair (evalQuotedDatum x) (evalQuotedDatums xs b)

-- set expression (without scopes)
evalSet :: Expression -> Env -> IO (LispVal, Env)
evalSet (Set name expr) env = do
  (expr', env') <- evalExpr expr env
  pure (Void, envSet env' name expr')
evalSet _ _ = throwException "Set: not supported"

-- eval application
evalApplicationArgs :: [Expression] -> Env -> IO ([LispVal], Env)
evalApplicationArgs [] env = return ([], env)
evalApplicationArgs [arg] env = do
  (arg', env') <- evalExpr arg env
  return ([arg'], env')
evalApplicationArgs (arg : args) env = do
  (arg', env') <- evalExpr arg env
  (args', env'') <- evalApplicationArgs args env'
  return (arg' : args', env'')

-- -- execute procedure
evalExactFormal :: [String] -> [LispVal] -> Env -> IO ([LispVal], Env)
evalExactFormal [] args env = pure (args, env)
evalExactFormal _ [] _ =
  throwException "Lambda procedure: incorrect argument count"
evalExactFormal (key : keys) (arg : args) env =
  evalExactFormal keys args env'
  where
    env' = envDef env key arg

evalVariadicFormal :: String -> [LispVal] -> Env -> IO Env
evalVariadicFormal key values env = pure $ envDef env key (apply values)
  where
    apply = foldr Hal.Evaluator.LispVal.Pair Hal.Evaluator.LispVal.Empty

evalFormal :: Formal -> [LispVal] -> Env -> IO Env
evalFormal (Exact keys) args env = do
  (args', env') <- evalExactFormal keys args env
  case args' of
    [] -> pure env'
    _ -> throwException "Lambda procedure: incorrect argument count"
evalFormal (Variadic keys key) args env = do
  (args', env') <- evalExactFormal keys args env
  evalVariadicFormal key args' env'

evalProcedure :: Procedure -> [LispVal] -> Env -> IO (LispVal, Env)
evalProcedure (NativeP procedure) args env = procedure args env
evalProcedure (LambdaP formal body) args env = do
  env' <- evalFormal formal args env
  evalExprs (toList body) env'

evalApplication :: Expression -> Env -> IO (LispVal, Env)
evalApplication (Application p args) env = do
  (procedure, env') <- evalExpr p env
  case procedure of
    (Hal.Evaluator.LispVal.Procedure procedure') -> do
      (args', env'') <- evalApplicationArgs args env'
      (res, env''') <- evalProcedure procedure' args' (newScope env'')
      pure (res, deleteScope env''')
    _ -> throwException $ "attempt to apply non-procedure " ++ show procedure
evalApplication _ _ = throwException "Application: not supported"

-- eval constant
evalConst :: Constant -> Env -> IO (LispVal, Env)
evalConst (Hal.Evaluator.Form.Number i) env = pure (Hal.Evaluator.LispVal.Number i, env)
evalConst (Hal.Evaluator.Form.Char c) env = pure (Hal.Evaluator.LispVal.Char c, env)
evalConst (Hal.Evaluator.Form.Float i) env = pure (Hal.Evaluator.LispVal.Float i, env)
evalConst (Hal.Evaluator.Form.String s) env = pure (Hal.Evaluator.LispVal.String s, env)
evalConst (Hal.Evaluator.Form.Bool b) env = pure (Hal.Evaluator.LispVal.Bool b, env)

-- begin expressions
evalBeginExpr :: Expression -> Env -> IO (LispVal, Env)
evalBeginExpr (BeginExpr exprs) env = evalExprs exprs env
evalBeginExpr _ _ = throwException "Begin: not supported"

-- load expressions
readFileContent :: FilePath -> IO String
readFileContent filePath = do
  file <- try (readFile filePath) :: IO (Either SomeException String)
  case file of
    Left err -> throwException $ show err
    Right content -> pure content

loadPath :: String -> Env -> IO (LispVal, Env)
loadPath path env = do
  expr <- readFileContent path
  case parseStringToDatums expr of
    Nothing -> throwException "Parsing error"
    Just datums -> case datumsToForms datums of
      Just forms -> evalForms forms env
      Nothing -> throwException "Parsing error"

evalLoad :: Expression -> Env -> IO (LispVal, Env)
evalLoad path env = do
  (path', env') <- evalExpr path env
  case path' of
    (Hal.Evaluator.LispVal.String path'') -> do
      res <- try (loadPath path'' env') :: IO (Either HalException (LispVal, Env))
      case res of
        Left (HalException err) -> throwException $ "load: " ++ err
        Right res' -> pure res'
    _ -> pure (Void, env')

-- eval expression
evalExpr :: Expression -> Env -> IO (LispVal, Env)
evalExpr expr@If2 {} env = evalIf expr env
evalExpr expr@If3 {} env = evalIf expr env
evalExpr cond@Cond {} env = evalCond cond env
evalExpr app@Application {} env = evalApplication app env
evalExpr lambda@Lambda {} env = evalLambda lambda env
evalExpr exprs@BeginExpr {} env = evalBeginExpr exprs env
evalExpr (Quote q) env = pure (evalQuotedDatum q, env)
evalExpr set@Set {} env = evalSet set env
evalExpr (Const c) env = evalConst c env
evalExpr (Load path) env = evalLoad path env
evalExpr (Var v) env = case envGet env v of
  Just val -> pure (val, env)
  _ -> case builtinGet v of
    Just procedure -> pure (Procedure procedure, env)
    _ -> throwException $ "variable " ++ v ++ " is not bound"

evalExprs :: [Expression] -> Env -> IO (LispVal, Env)
evalExprs [] env = pure (Void, env)
evalExprs [expr] env = evalExpr expr env
evalExprs (expr : exprs) env = do
  (val, env') <- evalExpr expr env
  evalExprs exprs env'

-- begin definitions
evalBeginDef :: Definition -> Env -> IO (LispVal, Env)
evalBeginDef (BeginDef defs) env = do
  env' <- evalDefs defs env
  pure (Void, env')
evalBeginDef _ _ = throwException "Begin: not supported"

-- eval define
evalDef :: Definition -> Env -> IO Env
evalDef (Define name (Just def)) env = do
  (val, env') <- evalExpr def env
  return $ envDef env name val
evalDef (Define name Nothing) env = pure $ envDef env name Void
evalDef (BeginDef []) env = pure env
evalDef (BeginDef [def]) env = evalDef def env
evalDef (BeginDef (def : defs)) env = do
  env' <- evalDef def env
  evalDef (BeginDef defs) env'

evalDefs :: [Definition] -> Env -> IO Env
evalDefs [] env = pure env
evalDefs [def] env = evalDef def env
evalDefs (def : defs) env = do
  env' <- evalDef def env
  evalDefs defs env'

-- eval form
evalForm :: Form -> Env -> IO (LispVal, Env)
evalForm (Definition def) env = do
  env' <- evalDef def env
  pure (Void, env')
evalForm (Expression expr) env = evalExpr expr env

-- forms -> eval
evalForms :: [Form] -> Env -> IO (LispVal, Env)
evalForms [] env = pure (Void, env)
evalForms [form] env = evalForm form env
evalForms (form : forms) env = do
  (val, env') <- evalForm form env
  evalForms forms env'

-- datums -> forms -> eval
evalDatums :: [Datum] -> Env -> IO (LispVal, Env)
evalDatums datums env = case datumsToForms datums of
  Just forms -> evalForms forms env
  _ -> throwException "Parser error"
