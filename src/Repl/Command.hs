module Repl.Command where

import Data.Text.Short (ShortText)

import Lambda.Syntax (Expr)

data Command
  = Bind {name :: ShortText, value :: Expr}
  | Type {name :: ShortText}
  | Eval {expr :: Expr}
  | Unknown {name :: ShortText}
