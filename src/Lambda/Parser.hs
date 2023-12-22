module Lambda.Parser where

import Text.Megaparsec
import Text.Megaparsec.Char.Lexer qualified as Lex

import Lambda.Syntax
import Parser

expr :: Parser Expr
expr = do
  x <- atom
  xs <- many atom
  pure (foldl App x xs)

atom :: Parser Expr
atom = choice [parens expr, int, lam, var]

int :: Parser Expr
int = Num <$> lexeme (Lex.signed empty Lex.decimal)

lam :: Parser Expr
lam = do
  _ <- symbol "\\"
  (param, typ) <- parens do
    param <- name
    _ <- symbol ":"
    typ <- expr
    pure (param, typ)
  _ <- symbol "."
  body <- expr
  pure Lam {param, typ, body}

var :: Parser Expr
var = Var <$> name

name :: Parser Name
name = Name 0 <$> ident

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")
