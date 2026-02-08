module Eval (evalTerm, evalProgram, Env) where

import Data.Map (Map, insert, lookup)
import Parser (Program (..), Term (..))
import Prelude hiding (lookup)

data EValue = EArrow String Term Env | EInt Integer | EBool Bool

instance Show EValue where
  show (EArrow {}) = "<fn>"
  show (EInt i) = show i
  show (EBool b) = show b

type Env = Map String EValue

evalTerm :: Env -> Term -> Either String EValue
evalTerm env t = case t of
  TInt a -> Right (EInt a)
  TTrue -> Right (EBool True)
  TFalse -> Right (EBool False)
  TApp fun arg -> do
    funres <- evalTerm env fun
    case funres of
      EArrow var body closureEnv -> do
        argres <- evalTerm env arg
        evalTerm (insert var argres closureEnv) body
      _ -> Left "Not a function"
  TVar var -> case lookup var env of
    Just val -> Right val
    Nothing -> Left "Variable not found"
  TSucc term -> do
    termres <- evalTerm env term
    case termres of
      EInt a -> Right (EInt (a + 1))
      _ -> Left "Not an integer"
  TPred term -> do
    termres <- evalTerm env term
    case termres of
      EInt a -> Right (EInt (a - 1))
      _ -> Left "Not an integer"
  TIsZero term -> do
    termres <- evalTerm env term
    case termres of
      EInt a -> Right (EBool (a == 0))
      _ -> Left "Not an integer"
  TIf cond yes no -> do
    condres <- evalTerm env cond
    case condres of
      EBool True -> evalTerm env yes
      EBool False -> evalTerm env no
      _ -> Left "Not a bool"
  TArrow var body -> Right (EArrow var body env)

evalProgram :: Env -> Program -> Either String (Maybe EValue, Env)
evalProgram env pl = case pl of
  PLVarDef name t -> do
    val <- evalTerm env t
    Right (Nothing, insert name val env)
  PLTerm t -> do
    val <- evalTerm env t
    Right (Just val, env)
