module Repl.Parser where

import Data.Char (isAlpha)
import Text.Megaparsec
import Text.Megaparsec.Char

import Lambda.Parser (expr)
import Parser
import Repl.Command
import qualified Data.Text.Short as ShortText

command :: Parser Command
command = choice [directive, eval] <* eof

directive :: Parser Command
directive = do
  _ <- char ':'
  name <- ShortText.fromText <$> lexeme (takeWhile1P Nothing isAlpha)
  case name of
    "let" -> do
      name <- lexeme ident
      _ <- symbol "="
      value <- expr
      pure Bind {name, value}
    "type" -> located (Type <$> ident)
    _ -> Unknown {name} <$ takeWhileP Nothing (const True)

eval :: Parser Command
eval = Eval <$> expr