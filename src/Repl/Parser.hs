module Repl.Parser (
  command,
) where

import Data.Char (isAlpha)
import Text.Megaparsec
import Text.Megaparsec.Char

import Data.Text.Short qualified as ShortText
import Lambda.Parser (expr)
import Parser
import Repl.Command

command :: Parser Command
command = choice [directive, eval] <* eof

directive :: Parser Command
directive = do
  _ <- char ':'
  (name, source) <- located do
    name <- takeWhile1P Nothing isAlpha
    pure (ShortText.fromText name,)
  case name of
    "let" -> do
      name <- lexeme ident
      _ <- symbol "="
      value <- expr
      pure Bind {name, value}
    "type" -> located (Type <$> ident)
    "help" -> Help {} <$ takeWhileP Nothing (const True)
    "quit" -> Quit {} <$ takeWhileP Nothing (const True)
    _ -> Unknown {name, source} <$ takeWhileP Nothing (const True)

eval :: Parser Command
eval = Eval <$> expr