module Parser where

import Data.Char (isAlphaNum)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Short (ShortText)
import Data.Text.Short qualified as ShortText
import Data.Void (Void)
import Text.Megaparsec (Parsec, empty, takeWhileP)
import Text.Megaparsec.Char (letterChar, space1)
import Text.Megaparsec.Char.Lexer qualified as Lex

type Parser = Parsec Void Text

ident :: Parser ShortText
ident = lexeme do
  c <- letterChar
  cs <- takeWhileP Nothing \x -> x == '\'' || isAlphaNum x
  let name = ShortText.pack (c : Text.unpack cs)
  pure name

lexeme :: Parser a -> Parser a
lexeme = Lex.lexeme sc

symbol :: Text -> Parser Text
symbol = Lex.symbol sc

sc :: Parser ()
sc = Lex.space space1 empty empty