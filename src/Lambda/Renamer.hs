module Lambda.Renamer where

import Control.Monad.RWS.CPS (RWS, asks, get, local, put, tell, runRWS)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Text.Short (ShortText)

import Lambda.Syntax

newtype Rename a = Rename (RWS [(ShortText, Name)] [ShortText] Int a)
  deriving newtype (Functor, Applicative, Monad)

runRename :: Int -> [(ShortText, Name)] -> Rename a -> Either (NonEmpty ShortText) a
runRename key env (Rename x) = case nonEmpty unbound of
  Nothing -> Right x'
  Just errors -> Left errors
  where
    (x', _key, unbound) = runRWS x env key

lookupName :: Name -> Rename Name
lookupName name = Rename do
  res <- asks (lookup name.name)
  case res of
    Nothing -> name <$ tell [name.name]
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
  Var {name} -> do
    name <- lookupName name
    pure Var {name}
  App {fun, arg} -> do
    fun <- expr fun
    arg <- expr arg
    pure App {fun, arg}
  Lam {param, typ, body} -> do
    typ <- expr typ
    (param, body) <- bindName param (expr body)
    pure Lam {param, typ, body}

intrinsicNames :: [(ShortText, Name)]
intrinsicNames = [("Int", Int), ("add", Add)]
