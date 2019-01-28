module Main where

import Prelude
import Data.List (intercalate)
import qualified Data.Text.IO as IO
import qualified Language.PureScript.CST.Lexer as CST
import qualified Language.PureScript.CST.Parser as CST
import qualified Language.PureScript.CST.Utils as CST
import qualified Language.PureScript.CST.Types as CST
import qualified Text.Megaparsec as M
import System.Environment (getArgs)

main :: IO ()
main = do
  file <- head <$> getArgs
  src <- IO.readFile file
  putStrLn file
  case CST.lex "Test.purs" src of
    Left err ->
      putStrLn $ M.errorBundlePretty err
    Right res -> do
      case CST.runParser (CST.parseModule (fst res)) of
        Left (CST.ParserError toks alts msg) -> do
          let
            pos = case toks of
              ((CST.TokenAnn (CST.SourceRange (CST.SourcePos line col) _) _ _), tok) : _ ->
                show line <> ":" <> show col <> " " <> show tok
              _ -> "EOF"
          putStrLn $ unlines
            [ "Parse error at " <> pos
            , msg
            , case alts of
                [] -> ""
                _  -> "Possibilities include: " <> intercalate ", " alts
            ]
        Right _ ->
          putStrLn "[OK]"

