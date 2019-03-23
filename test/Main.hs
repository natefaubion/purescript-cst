import Prelude

import qualified Data.ByteString.Lazy as BS
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString, findByExtension)
import Test.Tasty.QuickCheck
import Text.Read (readMaybe)
import Language.PureScript.CST.Errors as CST
import Language.PureScript.CST.Lexer as CST
import Language.PureScript.CST.Print as CST
import Language.PureScript.CST.Types
import System.FilePath (takeBaseName, replaceExtension)

main :: IO ()
main = do
  lytTests <- layoutTests
  defaultMain $ testGroup "CST"
    [ lytTests
    , litTests
    ]

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

litTests :: TestTree
litTests = testGroup "Literals"
  [ testProperty "Integer" $
      checkTok checkReadNum (\case TokInt a b -> Just (a, b); _ -> Nothing) . unInt
  , testProperty "Hex" $
      checkTok checkReadNum (\case TokInt a b -> Just (a, b); _ -> Nothing) . unHex
  , testProperty "Number" $
      checkTok checkReadNum (\case TokNumber a b -> Just (a, b); _ -> Nothing) . unFloat
  , testProperty "Exponent" $
      checkTok checkReadNum (\case TokNumber a b -> Just (a, b); _ -> Nothing) . unExponent
  ]

checkTok
  :: (Text -> a -> Gen Bool)
  -> (Token -> Maybe (Text, a))
  -> Text
  -> Gen Bool
checkTok p f t = case CST.lex t of
  Right (SourceToken _ tok : _)
    | Just (a, b) <- f tok ->
        if a == t
          then p t b
          else fail $ "Mismatched raw text: " <> show a
  Right toks ->
    fail $ "Failed to lex correctly: " <> show toks
  Left errs ->
    fail $ "Failed to parse: " <> unlines (CST.prettyPrintError <$> errs)

checkReadNum :: (Eq a, Read a) => Text -> a -> Gen Bool
checkReadNum t a = do
  let
    chs = case Text.unpack $ Text.replace ".e" ".0e" $ Text.replace "_" "" t of
      chs' | last chs' == '.' -> chs' <> "0"
      chs' -> chs'
  case (== a) <$> readMaybe chs of
    Just a' -> pure a'
    Nothing -> fail "Failed to `read`"

newtype PSSourceInt = PSSourceInt { unInt :: Text }
  deriving (Show, Eq)

instance Arbitrary PSSourceInt where
  arbitrary = resize 16 genInt

newtype PSSourceFloat = PSSourceFloat { unFloat :: Text }
  deriving (Show, Eq)

instance Arbitrary PSSourceFloat where
  arbitrary = resize 16 genFloat

newtype PSSourceExponent = PSSourceExponent { unExponent :: Text }
  deriving (Show, Eq)

instance Arbitrary PSSourceExponent where
  arbitrary = PSSourceExponent <$> do
    floatPart <- unFloat <$> resize 5 genFloat
    signPart <- fromMaybe "" <$> elements [ Just "+", Just "-", Nothing ]
    expPart <- unInt <$> resize 1 genInt
    pure $ floatPart <> "e" <> signPart <> expPart

newtype PSSourceHex = PSSourceHex { unHex :: Text }
  deriving (Show, Eq)

instance Arbitrary PSSourceHex where
  arbitrary = resize 16 genHex

genInt :: Gen PSSourceInt
genInt = PSSourceInt . Text.pack <$> do
  (:) <$> nonZeroChar
      <*> listOf numChar

genFloat :: Gen PSSourceFloat
genFloat = PSSourceFloat <$> do
  intPart <- unInt <$> genInt
  floatPart <- Text.pack <$> listOf1 numChar
  pure $ intPart <> "." <> floatPart

genHex :: Gen PSSourceHex
genHex = PSSourceHex <$> do
  nums <- listOf1 $ elements $ ['a'..'f'] <> ['A'..'F'] <> ['0'..'9']
  pure $ "0x" <> Text.pack nums

numChar :: Gen Char
numChar = elements "0123456789_"

nonZeroChar :: Gen Char
nonZeroChar = elements "123456789"
