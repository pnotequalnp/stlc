{-# OPTIONS_GHC -Wno-orphans #-}

module Parser where

import Data.Char (isAlphaNum)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Short (ShortText)
import Data.Text.Short qualified as ShortText
import Data.Void (Void)
import Error.Diagnose.Compat.Megaparsec (HasHints (..))
import Text.Megaparsec (Parsec, Pos, SourcePos (..), empty, getSourcePos, takeWhileP, unPos)
import Text.Megaparsec.Char (letterChar, space1)
import Text.Megaparsec.Char.Lexer qualified as Lex

import Lambda.Syntax (Span (..))

type Parser = Parsec Void Text

instance HasHints Void msg where
  hints = \case {}

ident :: Parser ShortText
ident = do
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

located :: Parser (Span -> a) -> Parser a
located p = lexeme do
  open <- getSourcePos
  f <- p
  close <- getSourcePos
  pure (f (toSpan open close))

toSpan :: SourcePos -> SourcePos -> Span
toSpan
  SourcePos {sourceName = filename, sourceLine = Pos startLine, sourceColumn = Pos startCol}
  SourcePos {sourceLine = Pos endLine, sourceColumn = Pos endCol} =
    Span {startLine, startCol, endLine, endCol, filename}

pattern Pos :: Int -> Pos
pattern Pos x <- (unPos -> x)
{-# COMPLETE Pos #-}
