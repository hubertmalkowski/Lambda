module Parser (pl, Term (..), Program (..)) where

import Data.Char
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

data Term
  = TArrow String Term
  | TInt Integer
  | TTrue
  | TFalse
  | TApp Term Term
  | TVar String
  | TSucc Term
  | TPred Term
  | TIsZero Term
  | TIf Term Term Term

instance Show Term where
  show (TArrow n t) = "λ" ++ n ++ "." ++ show t
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

-- Helper for reserved words
rWord :: String -> Parser ()
rWord w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

reservedWords :: [String]
reservedWords = ["if", "then", "else", "true", "false", "succ", "pred", "iszero", "fn", "let", "λ"]

validChar :: Parser Char
validChar = satisfy (\c -> isAlpha c && c /= 'λ')

identifier :: Parser String
identifier = (lexeme . try) $ do
  name <- some validChar
  if name `elem` reservedWords
    then fail $ "keyword " ++ show name ++ " cannot be used as an identifier"
    else return name

lambda :: Parser Term
lambda = do
  var <- between (symbol "λ") dot identifier
  TArrow var <$> term

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
      atom,
      lambda,
      ifelse,
      TSucc <$ rWord "succ" <*> term,
      TPred <$ rWord "succ" <*> term,
      TIsZero <$ rWord "iszero" <*> term
    ]

atom :: Parser Term
atom =
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
