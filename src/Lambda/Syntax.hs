module Lambda.Syntax where

import Data.Int (Int64)
import Data.Text (Text)
import Data.Text.Short (ShortText)
import Data.Text.Short qualified as ShortText

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
  = Num {value :: Int64}
  | Var {name :: Name}
  | App {fun :: Expr, arg :: Expr}
  | Lam {param :: Name, typ :: Expr, body :: Expr}