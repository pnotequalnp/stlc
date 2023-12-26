module Lambda.Syntax (
  -- * Names
  Name (..),
  pattern Type,
  pattern Int,
  pattern Add,
  pattern Sub,
  pattern Mul,
  renderName,

  -- * Expressions
  Expr (..),

  -- * Source
  Span (..),
  spanPosition,
)
where

import Data.Int (Int64)
import Data.Text (Text)
import Data.Text.Short (ShortText)
import Data.Text.Short qualified as ShortText
import Error.Diagnose (Position (..))

data Name
  = Name {key :: !Int, name :: ShortText}
  | Intrinsic {name :: ShortText}
  deriving stock (Eq, Show)

renderName :: Name -> Text
renderName = ShortText.toText . (.name)

pattern Type, Int, Add, Sub, Mul :: Name
pattern Type = Intrinsic {name = "Type"}
pattern Int = Intrinsic {name = "Int"}
pattern Add = Intrinsic {name = "add"}
pattern Sub = Intrinsic {name = "sub"}
pattern Mul = Intrinsic {name = "mul"}

data Expr
  = Num {value :: Int64, source :: Span}
  | Var {name :: Name, source :: Span}
  | App {fun :: Expr, arg :: Expr, source :: Span}
  | Lam {param :: Name, typ :: Expr, body :: Expr, source :: Span}
  | Arr {input :: Expr, output :: Expr, source :: Span}

data Span = Span
  { startLine :: !Int
  , startCol :: !Int
  , endLine :: !Int
  , endCol :: !Int
  , filename :: FilePath
  }

spanPosition :: Span -> Position
spanPosition Span {startLine, startCol, endLine, endCol, filename} = Position {begin = (startLine, startCol), end = (endLine, endCol), file = filename}

instance Semigroup Span where
  Span {startLine, startCol, filename} <> Span {endLine, endCol} =
    Span {startLine, startCol, endLine, endCol, filename}
