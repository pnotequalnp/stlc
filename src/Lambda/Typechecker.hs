module Lambda.Typechecker where

import Control.Exception (Exception, throw)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (ReaderT (runReaderT), asks, local)
import Data.Text (Text)
import Data.Text.Lazy qualified as LazyText
import Data.Text.Lazy.Builder qualified as Builder
import Data.Text.Short qualified as ShortText

import Lambda.Syntax

data Type
  = TVar {name :: Name}
  | TFun {param :: Type, result :: Type}
  | TUnknown {}
  deriving stock (Eq)

(-->) :: Type -> Type -> Type
param --> result = TFun {param, result}
infixr 9 -->

renderType :: Type -> Text
renderType = LazyText.toStrict . Builder.toLazyText . go
  where
    go = \case
      TVar {name} -> Builder.fromText (ShortText.toText name.name)
      TFun {param, result} -> "(" <> go param <> " -> " <> go result <> ")"
      TUnknown {} -> "?"

data TypeError
  = Unification {expected :: Type, actual :: Type}
  | UnsupportedExpr {}

newtype Infer a = Infer (ReaderT [(Name, Type)] (Either TypeError) a)
  deriving newtype (Functor, Applicative, Monad)

runInfer :: [(Name, Type)] -> Infer a -> Either TypeError a
runInfer env (Infer x) = runReaderT x env

lookupVar :: Name -> Infer Type
lookupVar name = Infer do
  res <- asks (lookup name)
  case res of
    Nothing -> throw (UnboundVariable name)
    Just typ -> pure typ

bindVar :: Name -> Type -> Infer a -> Infer a
bindVar name typ (Infer x) = Infer do
  local ((name, typ) :) x

typeError :: TypeError -> Infer a
typeError = Infer . throwError

infer :: Expr -> Infer Type
infer = \case
  Num {} -> pure TVar {name = Int}
  Var {name} -> lookupVar name
  App {fun, arg} -> do
    fun <- infer fun
    arg <- infer arg
    case fun of
      TFun {param, result}
        | param == arg -> pure result
        | otherwise -> typeError Unification {expected = param, actual = arg}
      _ -> typeError Unification {expected = TFun {param = arg, result = TUnknown}, actual = fun}
  Lam {param, typ, body}
    | Var {name} <- typ -> do
        let paramt = TVar {name}
        result <- bindVar param paramt (infer body)
        pure TFun {param = paramt, result}
    | otherwise -> typeError UnsupportedExpr {}

intrinsicTypes :: [(Name, Type)]
intrinsicTypes = [(Add, TVar Int --> TVar Int --> TVar Int)]

newtype UnboundVariable = UnboundVariable {name :: Name}
  deriving stock (Show)

instance Exception UnboundVariable
