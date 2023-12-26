module Lambda.Renamer (
  -- * Types
  Rename (..),
  runRename,
  UnboundVariable (..),
  unboundVarReport,

  -- * Renaming
  expr,
  intrinsicNames,
) where

import Control.Monad.RWS.CPS (RWS, asks, get, local, put, runRWS, tell)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Text (Text)
import Data.Text.Short (ShortText)
import Data.Text.Short qualified as ShortText
import Error.Diagnose (Marker (..), Report (..))

import Lambda.Syntax

newtype Rename a = Rename (RWS [(ShortText, Name)] [UnboundVariable] Int a)
  deriving newtype (Functor, Applicative, Monad)

data UnboundVariable = UnboundVariable
  { name :: ShortText
  , source :: Span
  }

unboundVarReport :: UnboundVariable -> Report Text
unboundVarReport UnboundVariable {name, source} = Err Nothing ("unbound variable: " <> ShortText.toText name) [(spanPosition source, This "unbound variable")] []

runRename :: Int -> [(ShortText, Name)] -> Rename a -> Either (NonEmpty UnboundVariable) a
runRename key env (Rename x) = case nonEmpty unbound of
  Nothing -> Right x'
  Just errors -> Left errors
  where
    (x', _key, unbound) = runRWS x env key

lookupName :: Name -> Span -> Rename Name
lookupName name source = Rename do
  res <- asks (lookup name.name)
  case res of
    Nothing -> name <$ tell [UnboundVariable {name = name.name, source}]
    Just name -> pure name

bindName :: Name -> Rename a -> Rename (Name, a)
bindName name (Rename x) = Rename do
  let text = name.name
  key <- get
  put (key + 1)
  let name = Name {key, name = text}
  x <- local ((text, name) :) x
  pure (name, x)

expr :: Expr -> Rename Expr
expr = \case
  x@Num {} -> pure x
  Var {name, source} -> do
    name <- lookupName name source
    pure Var {name, source}
  App {fun, arg, source} -> do
    fun <- expr fun
    arg <- expr arg
    pure App {fun, arg, source}
  Lam {param, typ, body, source} -> do
    typ <- expr typ
    (param, body) <- bindName param (expr body)
    pure Lam {param, typ, body, source}
  Arr {input, output, source} -> do
    input <- expr input
    output <- expr output
    pure Arr {input, output, source}

intrinsicNames :: [(ShortText, Name)]
intrinsicNames = [("Type", Type), ("Int", Int), ("add", Add), ("sub", Sub), ("mul", Mul)]
