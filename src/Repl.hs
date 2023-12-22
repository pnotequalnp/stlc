module Repl where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import Data.Text.Short (ShortText)
import Data.Void (Void)
import Text.Megaparsec (ParseErrorBundle, runParser)

import Lambda.Renamer (runRename)
import Lambda.Renamer qualified as Renamer
import Lambda.Syntax (Name (..))
import Lambda.Typechecker (Type, TypeError, infer, runInfer)
import Repl.Command (Command (..))
import Repl.Parser qualified as Parser
import Lambda.Eval (Value, runEval, eval)

data Result
  = Value {typ :: Type, value :: Value}
  | Binding {name :: ShortText, typ :: Type, value :: Value}
  | Typing {name :: ShortText, typ :: Type}
  | UnknownCommand {name :: ShortText}
  | ParseError {errors :: ParseErrorBundle Text Void}
  | UnboundVars {unbound :: NonEmpty ShortText}
  | TypeError {typeError :: TypeError}

command :: Int -> [(ShortText, Name)] -> [(Name, Type)] -> [(Name, Value)] -> Text -> Result
command key names types values s = case runParser Parser.command "" s of
  Left errors -> ParseError {errors}
  Right cmd -> case cmd of
    Unknown {name} -> UnknownCommand {name}
    Type {name} -> case lookup name names of
      Just name | Just typ <- lookup name types -> Typing {name = name.name, typ}
      _ -> UnboundVars {unbound = NonEmpty.singleton name}
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
