module Parser (pl, Term (..), Program (..), TypeSig (..)) where

-- TODO: This is nearly identical to the lambda-untyped parser.
-- Need to figure out a way to have code sharing here

import Data.Char
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

data Term
  = TArrow String TypeSig Term
  | TInt Integer
  | TTrue
  | TFalse
  | TApp Term Term
  | TVar String
  | TSucc Term
  | TPred Term
  | TIsZero Term
  | TIf Term Term Term
  | TTypeAbs String Term
  | TTypeApp Term TypeSig

instance Show Term where
  show (TArrow n sig t) = "λ" ++ n ++ " : " ++ show sig ++ "." ++ show t
  show (TInt i) = show i
  show TTrue = "true"
  show TFalse = "false"
  show (TApp (TVar n) (TVar p)) = n ++ " " ++ p
  show (TApp t (TVar p)) = "(" ++ show t ++ ")" ++ " " ++ p
  show (TApp (TVar t) p) = t ++ " " ++ "(" ++ show p ++ ")"
  show (TApp t p) = "(" ++ show t ++ ")" ++ " " ++ "(" ++ show p ++ ")"
  show (TVar n) = n
  show (TSucc n) = "succ " ++ "(" ++ show n ++ ")"
  show (TPred n) = "pred " ++ "(" ++ show n ++ ")"
  show (TIsZero n) = "pred " ++ "(" ++ show n ++ ")"
  show (TIf cond yes no) = "if " ++ show cond ++ " then " ++ show yes ++ " else " ++ show no
  show (TTypeAbs var body) = "Λ" ++ var ++ "." ++ show body
  show (TTypeApp (TVar n) sig) = n ++ "[" ++ show sig ++ "]"
  show (TTypeApp n sig) = "(" ++ show n ++ ")" ++ "[" ++ show sig ++ "]"

data TypeSig
  = TSArrow TypeSig TypeSig
  | TSVar String
  | TSForall String TypeSig

instance Show TypeSig where
  show (TSArrow from (TSArrow ffrom fto)) = show from ++ " -> " ++ "(" ++ show (TSArrow ffrom fto) ++ ")"
  show (TSArrow from to) = show from ++ " -> " ++ show to
  show (TSVar var) = var
  show (TSForall var sig) = "∀" ++ var ++ "." ++ show sig

data Program = PLVarDef String Term | PLTerm Term deriving (Show)

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
  _ <- symbol "λ"
  var <- identifier
  _ <- symbol ":"
  sig <- typesig
  _ <- dot
  TArrow var sig <$> term

typeAbstraction :: Parser Term
typeAbstraction = do
  _ <- symbol "Λ"
  var <- identifier
  _ <- dot
  TTypeAbs var <$> term

ifelse :: Parser Term
ifelse = do
  _ <- rWord "if"
  cond <- term
  _ <- rWord "then"
  yes <- term
  _ <- rWord "else"
  TIf cond yes <$> term

term :: Parser Term
term =
  choice
    [ app,
      lambda,
      typeAbstraction,
      ifelse,
      TSucc <$ rWord "succ" <*> term,
      TPred <$ rWord "succ" <*> term,
      TIsZero <$ rWord "iszero" <*> term
    ]

atom :: Parser Term
atom = typeApp

typeApp :: Parser Term
typeApp = do
  base <- atomBase
  sigs <- many (brackets typesig)
  return $ foldl TTypeApp base sigs

atomBase :: Parser Term
atomBase =
  choice
    [ TTrue <$ rWord "true",
      TFalse <$ rWord "false",
      TInt <$> integer,
      TVar <$> identifier,
      parens term
    ]

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
  return $ foldl1 TApp atoms

typesig :: Parser TypeSig
typesig = arrowTypesig

arrowTypesig :: Parser TypeSig
arrowTypesig = do
  from <- atomTypesig
  maybeTo <- optional (symbol "->" *> typesig)
  case maybeTo of
    Nothing -> return from
    Just to -> return $ TSArrow from to

atomTypesig :: Parser TypeSig
atomTypesig =
  choice
    [ forallTypesig,
      identifierTypesig,
      parens typesig
    ]

identifierTypesig :: Parser TypeSig
identifierTypesig = TSVar <$> identifier

forallTypesig :: Parser TypeSig
forallTypesig = do
  _ <- symbol "∀"
  var <- identifier
  _ <- dot
  TSForall var <$> typesig
