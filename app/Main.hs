module Main where

import Prelude
import Control.Monad (when)
import Data.Foldable (for_, find)
import qualified Data.Text.IO as IO
import qualified Language.PureScript.CST.Lexer as CST
import qualified Language.PureScript.CST.Monad as CST
import qualified Language.PureScript.CST.Parser as CST
import qualified Language.PureScript.CST.Print as CST
import System.Environment (getArgs)
import Text.Pretty.Simple (pPrint)

main :: IO ()
main = do
  args <- getArgs
  let file = case find ((/= '-') . head) args of
        Nothing -> error "Filepath required."
        Just a  -> a
  src <- IO.readFile file
  when ("-t" `elem` args) $ do
    let toks = either (const []) id $ CST.lex src
    IO.putStrLn $ CST.printTokens toks
    putStrLn "----------"
  case CST.parse src of
    Left errs ->
      for_ errs $ \err -> do
        putStrLn $ ex <> " " <> file <> " " <> CST.prettyPrintError err
    Right m -> do
      when ("-m" `elem` args) $ do
        pPrint m
      putStrLn $ check <> " " <> file

check :: String
check = "\x1b[32m✓\x1b[0m"

ex :: String
ex = "\x1b[31m✗\x1b[0m"
