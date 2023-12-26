module Repl.Command where

import Data.Text.Short (ShortText)

import Lambda.Syntax (Expr, Span)

data Command
  = Bind {name :: ShortText, value :: Expr}
  | Type {name :: ShortText, source :: Span}
  | Eval {expr :: Expr}
  | Help {}
  | Quit {}
  | Unknown {name :: ShortText, source :: Span}
