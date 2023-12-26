module Lambda.Eval (
  -- * Types
  Eval (..),
  runEval,
  Value (..),
  renderValue,
  RuntimeError (..),

  -- * Eval
  eval,
  intrinsicValues,
) where

import Control.Exception (Exception, throw)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text.Lazy qualified as LazyText
import Data.Text.Lazy.Builder qualified as Builder
import Data.Text.Lazy.Builder.Int qualified as Builder

import Lambda.Syntax

newtype Eval a = Eval ([(Name, Value)] -> a)
  deriving newtype (Functor, Applicative, Monad)

runEval :: [(Name, Value)] -> Eval a -> a
runEval env (Eval f) = f env

data Value
  = Integer Int64
  | Closure (Value -> Eval Value)

renderValue :: Value -> Text
renderValue = LazyText.toStrict . Builder.toLazyText . go
  where
    go = \case
      Integer x -> Builder.decimal x
      Closure _ -> "<function>"

lookupVar :: Name -> Eval Value
lookupVar name = Eval \env -> case lookup name env of
  Nothing -> throw UnboundVariable
  Just value -> value

bindVar :: Name -> Value -> Eval a -> Eval a
bindVar name value (Eval f) = Eval \env -> f ((name, value) : env)

eval :: Expr -> Eval Value
eval = \case
  Num {value} -> pure (Integer value)
  Var {name} -> lookupVar name
  App {fun, arg} -> do
    fun <- eval fun
    case fun of
      Closure f -> eval arg >>= f
      _ -> typeError
  Lam {param, body} -> pure $ Closure \arg -> bindVar param arg (eval body)
  Arr {} -> throw UnsupportedExpr

intrinsicValues :: [(Name, Value)]
intrinsicValues =
  [ (Add, arith (+))
  , (Sub, arith (-))
  , (Mul, arith (*))
  ]
  where
    arith (<+>) = Closure \case
      Integer x -> pure $ Closure \case
        Integer y -> pure (Integer (x <+> y))
        _ -> typeError
      _ -> typeError

data RuntimeError
  = TypeError
  | UnboundVariable
  | UnsupportedExpr
  deriving stock (Show)

typeError :: Eval Value
typeError = throw TypeError

instance Exception RuntimeError
