import Prelude

import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString, findByExtension)
import Language.PureScript.CST.Errors as CST
import Language.PureScript.CST.Lexer as CST
import Language.PureScript.CST.Print as CST
import System.FilePath (takeBaseName, replaceExtension)

main :: IO ()
main = defaultMain =<< layoutTests

layoutTests :: IO TestTree
layoutTests = do
  pursFiles <- findByExtension [".purs"] "./test/layout"
  return $ testGroup "Layout golden tests" $ do
    file <- pursFiles
    pure $ goldenVsString
      (takeBaseName file)
      (replaceExtension file ".out")
      (BS.fromStrict . Text.encodeUtf8 <$> runLexer file)

  where
  runLexer file = do
    src <- Text.readFile file
    case CST.lex src of
      Left errs ->
        pure $ Text.pack $ unlines $ CST.prettyPrintError <$> errs
      Right toks -> do
        pure $ CST.printTokens toks
