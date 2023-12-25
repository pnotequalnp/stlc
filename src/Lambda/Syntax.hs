module Lambda.Syntax where

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

pattern Type :: Name
pattern Type = Intrinsic {name = "Type"}

pattern Int :: Name
pattern Int = Intrinsic {name = "Int"}

pattern Add :: Name
pattern Add = Intrinsic {name = "add"}

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
