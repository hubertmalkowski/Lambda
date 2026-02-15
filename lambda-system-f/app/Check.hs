{- HLINT ignore "Use newtype instead of data" -}
module Check (Typ (..), checkProgram, Env, initEnv) where

import Data.Map (Map, insert, lookup)
import qualified Data.Map as Map (empty)
import Data.Set (Set)
import qualified Data.Set as Set (empty, insert, member)
import Parser (Program (..), Term (..), TypeSig (..), termPos)
import Text.Megaparsec (SourcePos)
import Prelude hiding (lookup)

data Typ
  = TTArrow Typ Typ
  | TTInt
  | TTBool
  | TTVar String
  | TTForall String Typ
  deriving (Eq)

instance Show Typ where
  show TTInt = "Int"
  show TTBool = "Bool"
  show (TTVar n) = n
  show (TTArrow (TTArrow from fto) to) = "(" ++ show (TTArrow from fto) ++ ")" ++ " -> " ++ show to
  show (TTArrow from to) = show from ++ " -> " ++ show to
  show (TTForall var typ) = "∀" ++ var ++ "." ++ show typ

sigToType :: Env -> TypeSig -> Either (SourcePos, String) Typ
sigToType env sig = case sig of
  TSArrow _ from to -> TTArrow <$> sigToType env from <*> sigToType env to
  TSVar _ "Int" -> Right TTInt
  TSVar _ "Bool" -> Right TTBool
  TSVar pos name -> if Set.member name (typeVars env) then Right (TTVar name) else Left (pos, "Type " ++ name ++ " is not defined in this scope")
  TSForall _ name sig' -> TTForall name <$> sigToType env sig'

data Env = Env
  { vars :: Map String Typ,
    typeVars :: Set String
  }

initEnv :: Env
initEnv =
  Env
    { vars = Map.empty,
      typeVars = Set.empty
    }

-- (TypeVar, Typ to replace it with) -> OldType -> NewType
substitute :: (String, Typ) -> Typ -> Typ
substitute (oldVar, newVar) oldType = case oldType of
  TTArrow from to -> TTArrow (substitute (oldVar, newVar) from) (substitute (oldVar, newVar) to)
  TTInt -> TTInt
  TTBool -> TTBool
  TTVar a -> if a == oldVar then newVar else TTVar a
  TTForall a body -> TTForall a (substitute (oldVar, newVar) body)

checkTerm :: Env -> Term -> Either (SourcePos, String) Typ
checkTerm env term = case term of
  TArrow _ name sig to -> do
    param <- sigToType env sig
    TTArrow param <$> checkTerm (env {vars = insert name param (vars env)}) to
  TInt _ _ -> Right TTInt
  TTrue _ -> Right TTBool
  TFalse _ -> Right TTBool
  TApp pos lambda param -> do
    tlambda <- checkTerm env lambda
    tparam <- checkTerm env param
    case tlambda of
      TTArrow from to -> if from == tparam then Right to else Left (termPos param, "Expected: " ++ show from ++ "; Got: " ++ show tparam)
      other -> Left (pos, "Expected: function, got: " ++ show other)
  TVar pos name -> case lookup name (vars env) of
    Just a -> Right a
    Nothing -> Left (pos, "Variable " ++ name ++ " not in scope")
  TSucc _ n -> do
    _ <- ensureInt env n
    Right TTInt
  TPred _ n -> do
    _ <- ensureInt env n
    Right TTInt
  TIsZero _ n -> do
    _ <- ensureInt env n
    Right TTBool
  TIf _ cond yes no -> do
    _ <- ensureBool env cond
    yesType <- checkTerm env yes
    noType <- checkTerm env no
    if yesType == noType then Right yesType else Left (termPos no, "No branch must match yes' branch type. Got: " ++ show noType ++ " Expected: " ++ show yesType)
  TTypeAbs pos var body -> do
    bodyType <- checkTerm (env {typeVars = Set.insert var (typeVars env)}) body
    if Set.member var (typeVars env)
      then
        Left (pos, "TypeVariable with name " ++ var ++ " already exists")
      else
        Right (TTForall var bodyType)
  TTypeApp _ abstraction sig -> do
    absType <- checkTerm env abstraction
    newType <- sigToType env sig
    case absType of
      TTForall var to -> Right (substitute (var, newType) to)
      other -> Left (termPos abstraction, "The term should be a type abstraction, instead got: " ++ show other)

ensureInt :: Env -> Term -> Either (SourcePos, String) Typ
ensureInt env term = do
  termType <- checkTerm env term
  case termType of
    TTInt -> Right TTInt
    other -> Left (termPos term, "Expected: " ++ show TTInt ++ " got: " ++ show other)

ensureBool :: Env -> Term -> Either (SourcePos, String) Typ
ensureBool env term = do
  termType <- checkTerm env term
  case termType of
    TTBool -> Right TTBool
    other -> Left (termPos term, "Expected: " ++ show TTBool ++ " got: " ++ show other)

checkProgram :: Env -> Program -> Either (SourcePos, String) (Typ, Env)
checkProgram env prog = case prog of
  PLVarDef name term -> do
    resType <- checkTerm env term
    Right (resType, env {vars = insert name resType (vars env)})
  PLTerm t -> do
    resType <- checkTerm env t
    Right (resType, env)
