module Lambda (
  -- * Types
  Name (..),
  Type (..),
  renderType,
  Value (..),
  renderValue,

  -- * Errors
  UnboundVariable (..),
  unboundVarReport,
  TypeError (..),
  typeErrorDiagnostic,

  -- * Intrinsics
  intrinsicNames,
  intrinsicTypes,
  intrinsicValues,

  -- * Utilities
  spanPosition,
) where

import Lambda.Eval (Value (..), intrinsicValues, renderValue)
import Lambda.Renamer (UnboundVariable (..), intrinsicNames, unboundVarReport)
import Lambda.Syntax (Name (..), spanPosition)
import Lambda.Typechecker (Type (..), TypeError (..), intrinsicTypes, renderType, typeErrorDiagnostic)
