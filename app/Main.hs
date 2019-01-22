module Main where

import Prelude
import Criterion.Main
import qualified Data.Text.IO as IO
import qualified Language.PureScript as PS
import qualified Language.PureScript.CST.Lexer as CST
import qualified Language.PureScript.CST.Parser as CST
import qualified Text.Megaparsec as M

main :: IO ()
main = do
  src <- IO.readFile "Test.purs"
  -- defaultMain
  --   [ bench "old" $ whnf (PS.lex "Test.purs") src
  --   , bench "new" $ whnf lex src
  --   ]
  case CST.lex "Test.purs" src of
    Left err ->
      putStrLn $ M.errorBundlePretty err
    Right res ->
      putStrLn $ show $ CST.parseModule $ fst res

