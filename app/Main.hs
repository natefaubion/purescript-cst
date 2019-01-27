module Main where

import Prelude
import qualified Data.Text.IO as IO
import qualified Language.PureScript.CST.Lexer as CST
import qualified Language.PureScript.CST.Parser as CST
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
      putStrLn $ show $ map snd $ fst res
      putStrLn "-----"
      putStrLn $ show $ CST.parseModule $ fst res
      -- let !(CST.Module {}) = CST.parseModule $ fst res
      -- putStrLn $ "[OK]"

