module Parser (pl, Term (..), Program (..), TypeSig (..), termPos) where

-- TODO: This is nearly identical to the lambda-untyped parser.
-- Need to figure out a way to have code sharing here

import Data.Char
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

data Term
  = TArrow SourcePos String TypeSig Term
  | TInt SourcePos Integer
  | TTrue SourcePos
  | TFalse SourcePos
  | TApp SourcePos Term Term
  | TVar SourcePos String
  | TSucc SourcePos Term
  | TPred SourcePos Term
  | TIsZero SourcePos Term
  | TIf SourcePos Term Term Term
  | TTypeAbs SourcePos String Term
  | TTypeApp SourcePos Term TypeSig

instance Show Term where
  show (TArrow _ n sig t) = "λ" ++ n ++ " : " ++ show sig ++ "." ++ show t
  show (TInt _ i) = show i
  show (TTrue _) = "true"
  show (TFalse _) = "false"
  show (TApp _ (TVar _ n) (TVar _ p)) = n ++ " " ++ p
  show (TApp _ t (TVar _ p)) = "(" ++ show t ++ ")" ++ " " ++ p
  show (TApp _ (TVar _ t) p) = t ++ " " ++ "(" ++ show p ++ ")"
  show (TApp _ t p) = "(" ++ show t ++ ")" ++ " " ++ "(" ++ show p ++ ")"
  show (TVar _ n) = n
  show (TSucc _ n) = "succ " ++ "(" ++ show n ++ ")"
  show (TPred _ n) = "pred " ++ "(" ++ show n ++ ")"
  show (TIsZero _ n) = "pred " ++ "(" ++ show n ++ ")"
  show (TIf _ cond yes no) = "if " ++ show cond ++ " then " ++ show yes ++ " else " ++ show no
  show (TTypeAbs _ var body) = "Λ" ++ var ++ "." ++ show body
  show (TTypeApp _ (TVar _ n) sig) = n ++ "[" ++ show sig ++ "]"
  show (TTypeApp _ n sig) = "(" ++ show n ++ ")" ++ "[" ++ show sig ++ "]"

data TypeSig
  = TSArrow SourcePos TypeSig TypeSig
  | TSVar SourcePos String
  | TSForall SourcePos String TypeSig

instance Show TypeSig where
  show (TSArrow _ from to@(TSArrow {})) = show from ++ " -> " ++ "(" ++ show to ++ ")"
  show (TSArrow _ from to) = show from ++ " -> " ++ show to
  show (TSVar _ var) = var
  show (TSForall _ var sig) = "∀" ++ var ++ "." ++ show sig

data Program = PLVarDef String Term | PLTerm Term deriving (Show)

termPos :: Term -> SourcePos
termPos (TArrow pos _ _ _) = pos
termPos (TInt pos _) = pos
termPos (TTrue pos) = pos
termPos (TFalse pos) = pos
termPos (TApp pos _ _) = pos
termPos (TVar pos _) = pos
termPos (TSucc pos _) = pos
termPos (TPred pos _) = pos
termPos (TIsZero pos _) = pos
termPos (TIf pos _ _ _) = pos
termPos (TTypeAbs pos _ _) = pos
termPos (TTypeApp pos _ _) = pos

sc :: Parser ()
sc = L.space space1 (L.skipBlockComment "(*" "*)") (L.skipLineComment "--")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

integer :: Parser Integer
integer = lexeme L.decimal

dot :: Parser String
dot = symbol "."

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

-- Helper for reserved words
rWord :: String -> Parser ()
rWord w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

reservedWords :: [String]
reservedWords = ["if", "then", "else", "true", "false", "succ", "pred", "iszero", "fn", "let", "λ", "Λ", "∀"]

validChar :: Parser Char
validChar = satisfy (\c -> isAlpha c && c /= 'λ' && c /= 'Λ' && c /= '∀')

identifier :: Parser String
identifier = (lexeme . try) $ do
  name <- some validChar
  if name `elem` reservedWords
    then fail $ "keyword " ++ show name ++ " cannot be used as an identifier"
    else return name

lambda :: Parser Term
lambda = do
  pos <- getSourcePos
  _ <- symbol "λ"
  var <- identifier
  _ <- symbol ":"
  sig <- typesig
  _ <- dot
  TArrow pos var sig <$> term

typeAbstraction :: Parser Term
typeAbstraction = do
  pos <- getSourcePos
  _ <- symbol "Λ"
  var <- identifier
  _ <- dot
  TTypeAbs pos var <$> term

ifelse :: Parser Term
ifelse = do
  pos <- getSourcePos
  _ <- rWord "if"
  cond <- term
  _ <- rWord "then"
  yes <- term
  _ <- rWord "else"
  TIf pos cond yes <$> term

term :: Parser Term
term =
  choice
    [ app,
      lambda,
      typeAbstraction,
      ifelse,
      succTerm,
      predTerm,
      isZeroTerm
    ]

succTerm :: Parser Term
succTerm = do
  pos <- getSourcePos
  _ <- rWord "succ"
  TSucc pos <$> term

predTerm :: Parser Term
predTerm = do
  pos <- getSourcePos
  _ <- rWord "pred"
  TPred pos <$> term

isZeroTerm :: Parser Term
isZeroTerm = do
  pos <- getSourcePos
  _ <- rWord "iszero"
  TIsZero pos <$> term

atom :: Parser Term
atom = typeApp

typeApp :: Parser Term
typeApp = do
  base <- atomBase
  sigs <- many (brackets typesig)
  return $ foldl typeAppWithPos base sigs

typeAppWithPos :: Term -> TypeSig -> Term
typeAppWithPos base = TTypeApp (termPos base) base

atomBase :: Parser Term
atomBase =
  choice
    [ trueTerm,
      falseTerm,
      intTerm,
      varTerm,
      parens term
    ]

trueTerm :: Parser Term
trueTerm = do
  pos <- getSourcePos
  _ <- rWord "true"
  return $ TTrue pos

falseTerm :: Parser Term
falseTerm = do
  pos <- getSourcePos
  _ <- rWord "false"
  return $ TFalse pos

intTerm :: Parser Term
intTerm = do
  pos <- getSourcePos
  TInt pos <$> integer

varTerm :: Parser Term
varTerm = do
  pos <- getSourcePos
  TVar pos <$> identifier

def :: Parser Program
def = do
  name <- identifier
  _ <- symbol ":="
  PLVarDef name <$> term

pl :: Parser Program
pl = try def <|> (PLTerm <$> term) <* eof

app :: Parser Term
app = do
  atoms <- some atom
  return $ foldl1 appWithPos atoms

appWithPos :: Term -> Term -> Term
appWithPos left = TApp (termPos left) left

typesig :: Parser TypeSig
typesig = arrowTypesig

arrowTypesig :: Parser TypeSig
arrowTypesig = do
  pos <- getSourcePos
  from <- atomTypesig
  maybeTo <- optional (symbol "->" *> typesig)
  case maybeTo of
    Nothing -> return from
    Just to -> return $ TSArrow pos from to

atomTypesig :: Parser TypeSig
atomTypesig =
  choice
    [ forallTypesig,
      identifierTypesig,
      parens typesig
    ]

identifierTypesig :: Parser TypeSig
identifierTypesig = do
  pos <- getSourcePos
  TSVar pos <$> identifier

forallTypesig :: Parser TypeSig
forallTypesig = do
  pos <- getSourcePos
  _ <- symbol "∀"
  var <- identifier
  _ <- dot
  TSForall pos var <$> typesig
