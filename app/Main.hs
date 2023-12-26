module Main (main) where

import Control.Monad (when)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Text.Short (ShortText)
import Data.Text.Short qualified as ShortText
import Error.Diagnose (Diagnostic, Marker (..), Note (..), Report (..), TabSize (..), WithUnicode (..), addFile, addReport, defaultStyle, printDiagnostic)
import Error.Diagnose.Compat.Megaparsec (errorDiagnosticFromBundle)
import System.Exit (exitSuccess)
import System.IO (hFlush, isEOF, stdout)

import Lambda (intrinsicNames, intrinsicTypes, intrinsicValues)
import Lambda.Eval (Value, renderValue)
import Lambda.Renamer (unboundVarReport)
import Lambda.Syntax (Name (..), spanPosition)
import Lambda.Typechecker (Type, renderType, typeErrorDiagnostic)
import Repl (Result (..), command)

main :: IO ()
main = loop 0 intrinsicNames intrinsicTypes intrinsicValues

loop :: Int -> [(ShortText, Name)] -> [(Name, Type)] -> [(Name, Value)] -> IO ()
loop key names types values = do
  Text.putStr "> "
  hFlush stdout
  eof <- isEOF
  when eof exitSuccess
  s <- Text.getLine
  let continue = loop key names types values
  case command key names types values s of
    _ | Text.null s -> continue
    Value {typ, value} -> do
      Text.putStr ": "
      Text.putStrLn (renderType typ)
      Text.putStrLn (renderValue value)
      continue
    Binding {name = text, typ, value} -> do
      Text.putStr (ShortText.toText text)
      Text.putStr " : "
      Text.putStrLn (renderType typ)
      let name = Name {key, name = text}
      loop key ((text, name) : names) ((name, typ) : types) ((name, value) : values)
    Typing {name, typ} -> do
      Text.putStr (ShortText.toText name)
      Text.putStr " : "
      Text.putStrLn (renderType typ)
      continue
    ShowHelp {} -> do
      Text.putStrLn "Available commands: help, quit, let, type"
      continue
    QuitRepl {} -> pure ()
    UnknownCommand {name, source} -> do
      let msg = "unknown command: " <> ShortText.toText name
          hint = Hint "use `:help` to see all commands"
          reports = addReport mempty (Err Nothing msg [(spanPosition source, This "unknown command")] [hint])
          diag = addFile reports "" (Text.unpack s)
      stdoutDiagnostic diag
      continue
    ParseError {errors} -> do
      let diag = addFile (errorDiagnosticFromBundle Nothing "parse error" Nothing errors) "" (Text.unpack s)
      stdoutDiagnostic diag
      continue
    UnboundVars {unbound} -> do
      let reports = foldl (\d u -> addReport d (unboundVarReport u)) mempty unbound
          diag = addFile reports "" (Text.unpack s)
      stdoutDiagnostic diag
      continue
    TypeError {typeError} -> do
      let diag = addFile (typeErrorDiagnostic typeError) "" (Text.unpack s)
      stdoutDiagnostic diag
      continue

stdoutDiagnostic :: Diagnostic Text -> IO ()
stdoutDiagnostic = printDiagnostic stdout WithUnicode (TabSize 2) defaultStyle
