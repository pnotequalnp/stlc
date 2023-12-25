module Main (main) where

import Control.Monad (when)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Text.Short qualified as ShortText
import Error.Diagnose (Diagnostic, TabSize (..), WithUnicode (..), addFile, defaultStyle, printDiagnostic, addReport)
import Error.Diagnose.Compat.Megaparsec (errorDiagnosticFromBundle)
import System.Exit (exitSuccess)
import System.IO (hFlush, isEOF, stderr, stdout)

import Data.Text (Text)
import Lambda (intrinsicNames, intrinsicTypes, intrinsicValues)
import Lambda.Eval (renderValue)
import Lambda.Syntax (Name (..))
import Lambda.Typechecker (renderType, typeErrorDiagnostic)
import Repl (Result (..), command)
import Lambda.Renamer (unboundVarReport)

main :: IO ()
main = loop 0 intrinsicNames intrinsicTypes intrinsicValues
  where
    loop key names types values = do
      Text.putStr "> "
      hFlush stdout
      eof <- isEOF
      when eof exitSuccess
      s <- Text.getLine
      if Text.null s
        then loop key names types values
        else do
          (key, newNames, newTypes, newValues) <- case command key names types values s of
            Value {typ, value} -> do
              Text.putStr ": "
              Text.putStrLn (renderType typ)
              Text.putStrLn (renderValue value)
              pure (key, [], [], [])
            Binding {name = text, typ, value} -> do
              Text.putStr (ShortText.toText text)
              Text.putStr " : "
              Text.putStrLn (renderType typ)
              let name = Name {key, name = text}
              pure (key + 1, [(text, name)], [(name, typ)], [(name, value)])
            Typing {name, typ} -> do
              Text.putStr (ShortText.toText name)
              Text.putStr " : "
              Text.putStrLn (renderType typ)
              pure (key, [], [], [])
            UnknownCommand {name} -> do
              Text.putStr "unknown command: "
              print name
              pure (key, [], [], [])
            ParseError {errors} -> do
              let diag = addFile (errorDiagnosticFromBundle Nothing "parse error" Nothing errors) "" (Text.unpack s)
              stderrDiagnostic diag
              pure (key, [], [], [])
            UnboundVars {unbound} -> do
              let reports = foldl (\d u -> addReport d (unboundVarReport u)) mempty unbound
                  diag = addFile reports "" (Text.unpack s)
              stderrDiagnostic diag
              pure (key, [], [], [])
            TypeError {typeError} -> do
              let diag = addFile (typeErrorDiagnostic typeError) "" (Text.unpack s)
              stderrDiagnostic diag
              pure (key, [], [], [])
          loop key (newNames <> names) (newTypes <> types) (newValues <> values)

stderrDiagnostic :: Diagnostic Text -> IO ()
stderrDiagnostic = printDiagnostic stderr WithUnicode (TabSize 2) defaultStyle
