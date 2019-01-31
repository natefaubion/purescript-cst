module Main where

import Prelude
import Data.List (intercalate)
import qualified Data.Text.IO as IO
import qualified Language.PureScript.CST.Lexer as CST
import qualified Language.PureScript.CST.Monad as CST
import qualified Language.PureScript.CST.Parser as CST
import qualified Language.PureScript.CST.Print as CST
import qualified Language.PureScript.CST.Types as CST
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
    Left (CST.ParserError pos tok alts msg) -> do
      putStrLn $ unlines
        [ "Parse error at " <> show (CST.srcLine pos) <> ":" <> show (CST.srcColumn pos)
        , msg
        , maybe "" (show . snd) tok
        , case alts of
            [] -> ""
            _  -> "Possibilities include: " <> intercalate ", " alts
        ]
    Right m -> do
      -- putStrLn $ show m
      putStrLn "[OK]"

