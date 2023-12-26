module Repl (
  -- * Types
  Result (..),

  -- * REPL
  command,
) where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import Data.Text.Short (ShortText)
import Text.Megaparsec (ParseErrorBundle, runParser)

import Lambda.Eval (Value, eval, runEval)
import Lambda.Renamer (UnboundVariable (..), runRename)
import Lambda.Renamer qualified as Renamer
import Lambda.Syntax (Name (..), Span)
import Lambda.Typechecker (Type, TypeError, infer, runInfer)
import Parser (ParseError)
import Parser qualified
import Repl.Command (Command (..))
import Repl.Parser qualified as Parser

data Result
  = Value {typ :: Type, value :: Value}
  | Binding {name :: ShortText, typ :: Type, value :: Value}
  | Typing {name :: ShortText, typ :: Type}
  | UnknownCommand {name :: ShortText, source :: Span}
  | ParseError {errors :: ParseErrorBundle Text ParseError}
  | UnboundVars {unbound :: NonEmpty UnboundVariable}
  | TypeError {typeError :: TypeError}
  | ShowHelp {}
  | QuitRepl {}

command :: Int -> [(ShortText, Name)] -> [(Name, Type)] -> [(Name, Value)] -> Text -> Result
command key names types values s = case runParser (Parser.sc *> Parser.command) "" s of
  Left errors -> ParseError {errors}
  Right cmd -> case cmd of
    Unknown {name, source} -> UnknownCommand {name, source}
    Help {} -> ShowHelp {}
    Quit {} -> QuitRepl {}
    Type {name, source} -> case lookup name names of
      Just name | Just typ <- lookup name types -> Typing {name = name.name, typ}
      _ -> UnboundVars {unbound = NonEmpty.singleton UnboundVariable {name, source}}
    Bind {name, value} -> case evalExpr value of
      Value {typ, value} -> Binding {name, typ, value}
      res -> res
    Eval {expr} -> evalExpr expr
  where
    evalExpr expr = case runRename key names (Renamer.expr expr) of
      Left unbound -> UnboundVars {unbound}
      Right expr -> case runInfer types (infer expr) of
        Left typeError -> TypeError {typeError}
        Right typ -> Value {typ, value = runEval values (eval expr)}
