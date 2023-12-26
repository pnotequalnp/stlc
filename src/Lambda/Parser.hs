module Lambda.Parser (
  -- * Parsers
  expr,
) where

import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Text.Megaparsec
import Text.Megaparsec.Char.Lexer qualified as Lex

import Lambda.Syntax
import Parser

expr :: Parser Expr
expr = makeExprParser term [[arrow]]
  where
    arrow = InfixR do
      _ <- symbol "->"
      pure \x y -> Arr {input = x, output = y, source = x.source <> y.source}

term :: Parser Expr
term = do
  x <- atom
  xs <- many atom
  pure (foldl app x xs)
  where
    app fun arg = App {fun, arg, source = fun.source <> arg.source}

atom :: Parser Expr
atom = choice [parens expr, int, lam, var]

int :: Parser Expr
int = located do
  Num <$> Lex.decimal

lam :: Parser Expr
lam = located do
  _ <- symbol "\\"
  (param, typ) <- parens do
    param <- lexeme name
    _ <- symbol ":"
    typ <- expr
    pure (param, typ)
  _ <- symbol "."
  body <- expr
  pure \source -> Lam {param, typ, body, source}

var :: Parser Expr
var = located do
  Var <$> name

name :: Parser Name
name = Name 0 <$> ident

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")
