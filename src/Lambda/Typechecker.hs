module Lambda.Typechecker where

import Control.Exception (Exception, throw)
import Control.Monad (unless)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (ReaderT (runReaderT), asks, local)
import Data.Text (Text)
import Data.Text.Lazy qualified as LazyText
import Data.Text.Lazy.Builder qualified as Builder
import Data.Text.Short qualified as ShortText
import Error.Diagnose (Diagnostic, Marker (..), Report (..), addReport)

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
renderType = LazyText.toStrict . Builder.toLazyText . go 0
  where
    go (p :: Int) = \case
      TVar {name} -> Builder.fromText (ShortText.toText name.name)
      TFun {param, result} -> parens p (go (p + 1) param <> " -> " <> go p result)
      TUnknown {} -> "?"
    parens 0 x = x
    parens _ x = "(" <> x <> ")"

data TypeError
  = Unification {expected :: Type, actual :: Type, source :: Span}
  | KindError {expected :: Type, actual :: Type, source :: Span}
  | UnsupportedExpr {source :: Span}
  | UnsupportedType {source :: Span}

typeErrorDiagnostic :: TypeError -> Diagnostic Text
typeErrorDiagnostic = \case
  Unification {expected, actual, source} -> uniError expected actual source
  KindError {expected, actual, source} -> uniError expected actual source
  UnsupportedExpr {source} -> addReport mempty (Err Nothing "syntax error" [(spanPosition source, This "type syntax in expr")] [])
  UnsupportedType {source} -> addReport mempty (Err Nothing "syntax error" [(spanPosition source, This "expression syntax in type")] [])
  where
    uniError expected actual source = addReport mempty (Err Nothing msg [(spanPosition source, This ptr)] [])
      where
        msg = "expected type: " <> renderType expected
        ptr = "actual type: " <> renderType actual

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
    funt <- infer fun
    argt <- infer arg
    case funt of
      TFun {param, result}
        | param == argt -> pure result
        | otherwise -> typeError Unification {expected = param, actual = argt, source = arg.source}
      _ -> typeError Unification {expected = TFun {param = argt, result = TUnknown}, actual = funt, source = fun.source}
  Lam {param, typ, body} -> do
    paramt <- kindcheck (TVar Type) typ
    result <- bindVar param paramt (infer body)
    pure TFun {param = paramt, result}
  Arr {source} -> typeError UnsupportedExpr {source}

kindinfer :: Expr -> Infer (Type, Type)
kindinfer = \case
  Var {name} -> do
    kind <- lookupVar name
    pure (TVar {name}, kind)
  Arr {input, output} -> do
    input <- kindcheck (TVar Type) input
    output <- kindcheck (TVar Type) output
    pure (TFun {param = input, result = output}, TVar Type)
  e -> typeError UnsupportedType {source = e.source}

kindcheck :: Type -> Expr -> Infer Type
kindcheck expected e = do
  (t, actual) <- kindinfer e
  unless (expected == actual) do
    typeError KindError {actual, expected, source = e.source}
  pure t

intrinsicTypes :: [(Name, Type)]
intrinsicTypes =
  [ (Type, TVar Type)
  , (Int, TVar Type)
  , (Add, TVar Int --> TVar Int --> TVar Int)
  , (Sub, TVar Int --> TVar Int --> TVar Int)
  , (Mul, TVar Int --> TVar Int --> TVar Int)
  ]

newtype UnboundVariable = UnboundVariable {name :: Name}
  deriving stock (Show)

instance Exception UnboundVariable
