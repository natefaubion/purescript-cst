module Main where

import Prelude
import Data.Foldable (for_)
import qualified Data.Text.IO as IO
import qualified Language.PureScript.CST.Lexer as CST
import qualified Language.PureScript.CST.Monad as CST
import qualified Language.PureScript.CST.Parser as CST
import qualified Language.PureScript.CST.Print as CST
import System.Environment (getArgs)

main :: IO ()
main = do
  file <- head <$> getArgs
  src <- IO.readFile file
  putStrLn file
  let toks = either (const []) id $ CST.lex src
  IO.putStrLn $ CST.printTokens toks
  putStrLn "----------"
  case CST.parse src of
    Left errs ->
      for_ errs $ \err -> do
        putStrLn $ CST.prettyPrintError err
    Right m -> do
      putStrLn $ show m
      putStrLn "[OK]"

