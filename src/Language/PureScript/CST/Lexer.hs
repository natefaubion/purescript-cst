module Language.PureScript.CST.Lexer where

import Prelude

import Control.Applicative ((<|>))
import Data.Char (isAscii, isSymbol)
import qualified Data.DList as DList
import Data.Foldable (foldl')
import Data.Text (Text)
import Data.Void (Void)
import qualified Data.Text as Text
import Language.PureScript.CST.Types (Token(..), TokenAnn(..), Comment(..), LineFeed(..), SourcePos(..), SourceStyle(..))
import Text.Megaparsec (MonadParsec)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P

type Lexer e m = MonadParsec e Text m

type LexerResult = ([(TokenAnn, Token)], [Comment LineFeed])

space :: Lexer e m => m Int
space = Text.length <$> P.takeWhile1P (Just "space") (== ' ')

line :: Lexer e m => m LineFeed
line = CRLF <$ P.crlf <|> LF <$ P.newline

lineComment :: Lexer e m => m Text
lineComment = do
  pre <- P.try $ P.chunk "--" <* P.notFollowedBy symbolChar
  (pre <>) . Text.pack <$> P.manyTill P.anySingle (P.lookAhead line)

blockComment :: Lexer e m => m Text
blockComment = do
  text <- P.chunk "{-" *> P.manyTill P.anySingle (P.chunk "-}")
  pure $ Text.pack ("{-" <> text <> "-}")

trailingComment :: Lexer e m => m (Comment void)
trailingComment = P.choice
  [ Space <$> space
  , Comment <$> (lineComment <|> blockComment)
  ]

breakComments :: Lexer e m => m ([Comment void], [Comment LineFeed])
breakComments = go mempty
  where
  go ts = do
    isLine <- True <$ P.lookAhead line <|> pure False
    if isLine
      then do
        ls <- P.many (Line <$> line <|> trailingComment)
        pure (DList.toList ts, ls)
      else do
        mbT <- P.optional trailingComment
        case mbT of
          Nothing -> pure (DList.toList ts, [])
          Just t -> go (DList.snoc ts t)

tokens :: Lexer e m => m LexerResult
tokens = do
  leading <- uncurry (<>) <$> breakComments
  mbTok <- P.optional token
  case mbTok of
    Just tok -> go mempty tok leading
    Nothing -> pure (mempty, leading)
  where
  go acc prevTok prevLeading = do
    (trailing, leading) <- breakComments
    let
      prevAnn = TokenAnn prevLeading trailing
      acc' = DList.snoc acc (prevAnn, prevTok)
    mbTok <- P.optional token
    case mbTok of
      Just tok -> go acc' tok leading
      Nothing -> pure (DList.toList acc', leading)

token :: Lexer e m => m Token
token = P.choice
  [ TokLeftParen   <$ P.char '('
  , TokRightParen  <$ P.char ')'
  , TokLeftBrace   <$ P.char '{'
  , TokRightBrace  <$ P.char '}'
  , TokLeftSquare  <$ P.char '['
  , TokRightSquare <$ P.char ']'
  , TokTick        <$ P.char '`'
  , TokComma       <$ P.char ','

  , P.try $ P.choice
      [ TokDoubleColon   ASCII   <$ P.string "::"
      , TokDoubleColon   Unicode <$ P.char '∷'
      , TokLeftArrow     ASCII   <$ P.string "<-"
      , TokLeftArrow     Unicode <$ P.char '←'
      , TokRightArrow    ASCII   <$ P.string "->"
      , TokRightArrow    Unicode <$ P.char '→'
      , TokLeftFatArrow  ASCII   <$ P.string "<="
      , TokLeftFatArrow  Unicode <$ P.char '⇐'
      , TokRightFatArrow ASCII   <$ P.string "=>"
      , TokRightFatArrow Unicode <$ P.char '⇒'
      , TokEquals                <$ P.char '='
      , TokColon                 <$ P.char ':'
      , TokPipe                  <$ P.char '|'
      , TokDot                   <$ P.char '.'
      ] <* P.notFollowedBy symbolChar

  , P.try $ TokUnderscore <$ (P.char '_' <* P.notFollowedBy identLetter)
  , P.label "hole" $ P.try $ TokHole <$> (P.char '?' *> lname)
  , P.label "proper identifier" $ TokUpperName <$> uname
  , P.label "identifier" $ TokLowerName <$> lname
  , P.label "symbol" $ TokSymbol <$> symbol
  , P.label "character" $ uncurry TokChar <$> charLiteral
  , P.label "raw string" $ TokRawString <$> rawStringLiteral
  , P.label "string" $ uncurry TokString <$> stringLiteral
  , P.label "integer" $ uncurry TokInt <$> intLiteral
  , P.label "number" $ uncurry TokNumber <$> numberLiteral
  ]

lname :: Lexer e m => m Text
lname = Text.cons <$> identStart <*> (Text.pack <$> P.many identLetter)

uname :: Lexer e m => m Text
uname = Text.cons <$> P.upperChar <*> (Text.pack <$> P.many identLetter)

symbol :: Lexer e m => m Text
symbol = P.takeWhile1P (Just "symbol") isSymbolChar

symbolChar :: Lexer e m => m Char
symbolChar = P.label "symbol" $ P.satisfy isSymbolChar

isSymbolChar :: Char -> Bool
isSymbolChar c = (c `elem` (":!#$%&*+./<=>?@\\^|-~" :: [Char])) || (not (isAscii c) && isSymbol c)

identStart :: Lexer e m => m Char
identStart = P.lowerChar <|> P.char '_'

identLetter :: Lexer e m => m Char
identLetter = P.alphaNumChar <|> P.char '_' <|> P.char '\''

validModuleName :: Text -> Bool
validModuleName s = '_' `notElemText` s

validUName :: Text -> Bool
validUName s = '\'' `notElemText` s

notElemText :: Char -> Text -> Bool
notElemText c = not . Text.any (== c)

charLiteral :: Lexer e m => m (Text, Char)
charLiteral = do
  (raw, ch) <- P.char '\'' *> P.match P.charLiteral <* P.char '\''
  if fromEnum ch > 0xFFFF
    then fail "astral code point in character literal; characters must be valid UTF-16 code units"
    else pure (raw, ch)

stringLiteral :: Lexer e m => m (Text, Text)
stringLiteral = do
  (raw, str) <- P.char '"' *> P.match (P.manyTill P.charLiteral (P.lookAhead (P.char '"'))) <* P.char '"'
  pure (raw, Text.pack str)

rawStringLiteral :: forall e m. Lexer e m => m Text
rawStringLiteral = delimiter *> go mempty
  where
  delimiter =
    P.string "\"\"\""

  go acc = do
    chs <- P.takeWhileP Nothing (\c -> c /= '"')
    let acc' = acc <> chs
    (delimiter *> pure acc)
      <|> (go . (acc' <>) =<< P.takeP Nothing 3)

intLiteral :: Lexer e m => m (Text, Integer)
intLiteral = P.match P.decimal

numberLiteral :: Lexer e m => m (Text, Double)
numberLiteral = P.match P.float

lex :: Text -> Text -> Either (P.ParseErrorBundle Text Void) LexerResult
lex name = P.parse (tokens <* P.eof) (Text.unpack name)

tokenDelta :: Token -> (Int, Int)
tokenDelta = \case
  TokLeftParen             -> (0, 1)
  TokRightParen            -> (0, 1)
  TokLeftBrace             -> (0, 1)
  TokRightBrace            -> (0, 1)
  TokLeftSquare            -> (0, 1)
  TokRightSquare           -> (0, 1)
  TokLeftArrow ASCII       -> (0, 2)
  TokLeftArrow Unicode     -> (0, 1)
  TokRightArrow ASCII      -> (0, 2)
  TokRightArrow Unicode    -> (0, 1)
  TokLeftFatArrow ASCII    -> (0, 2)
  TokLeftFatArrow Unicode  -> (0, 1)
  TokRightFatArrow ASCII   -> (0, 2)
  TokRightFatArrow Unicode -> (0, 1)
  TokColon                 -> (0, 1)
  TokDoubleColon ASCII     -> (0, 2)
  TokDoubleColon Unicode   -> (0, 1)
  TokEquals                -> (0, 1)
  TokPipe                  -> (0, 1)
  TokTick                  -> (0, 1)
  TokDot                   -> (0, 1)
  TokComma                 -> (0, 1)
  TokUnderscore            -> (0, 1)
  TokLowerName name        -> (0, Text.length name)
  TokUpperName name        -> (0, Text.length name)
  TokSymbol sym            -> (0, Text.length sym)
  TokHole hole             -> (0, Text.length hole + 1)
  TokChar raw _            -> (0, Text.length raw + 2)
  TokInt raw _             -> (0, Text.length raw)
  TokNumber raw _          -> (0, Text.length raw)
  TokString raw _          -> multiLine 1 $ textDelta raw
  TokRawString raw         -> multiLine 3 $ textDelta raw

multiLine :: Int -> (Int, Int) -> (Int, Int)
multiLine n (0, c) = (0, c + n + n)
multiLine n (l, c) = (l, c + n)

commentDelta :: (a -> (Int, Int)) -> Comment a -> (Int, Int)
commentDelta k = \case
  Comment raw -> textDelta raw
  Space n -> (0, n)
  Line a -> k a

lineDelta :: LineFeed -> (Int, Int)
lineDelta _ = (1, 1)

textDelta :: Text -> (Int, Int)
textDelta = Text.foldl' go (0, 0)
  where
  go (!l, !c) = \case
    '\n' -> (l + 1, 1)
    _    -> (l, c + 1)

applyDelta :: SourcePos -> (Int, Int) -> SourcePos
applyDelta (SourcePos l c) = \case
  (0, n) -> SourcePos l (c + n)
  (k, d) -> SourcePos (l + k) d

addSourcePos :: [(TokenAnn, Token)] -> [(SourcePos, TokenAnn, Token)]
addSourcePos = flip addSourcePos' (SourcePos 1 1)

addSourcePos' :: [(TokenAnn, Token)] -> SourcePos -> [(SourcePos, TokenAnn, Token)]
addSourcePos' = foldr go (const [])
  where
  go (ann, tok) k pos = do
    let
      pos'   = goLeading pos (tokLeadingComments ann)
      pos''  = applyDelta pos' (tokenDelta tok)
      pos''' = goTrailing pos'' (tokTrailingComments ann)
    (pos'', ann, tok) : k pos'''

  goLeading = foldl' $ \a b ->
    applyDelta a (commentDelta lineDelta b)

  goTrailing = foldl' $ \a b ->
    applyDelta a (commentDelta (const (0, 0)) b)

printToken :: Token -> Text
printToken = \case
  TokLeftParen             -> "("
  TokRightParen            -> ")"
  TokLeftBrace             -> "{"
  TokRightBrace            -> "}"
  TokLeftSquare            -> "["
  TokRightSquare           -> "]"
  TokLeftArrow ASCII       -> "<-"
  TokLeftArrow Unicode     -> "←"
  TokRightArrow ASCII      -> "->"
  TokRightArrow Unicode    -> "→"
  TokLeftFatArrow ASCII    -> "<="
  TokLeftFatArrow Unicode  -> "⇐"
  TokRightFatArrow ASCII   -> "=>"
  TokRightFatArrow Unicode -> "⇒"
  TokColon                 -> ":"
  TokDoubleColon ASCII     -> "::"
  TokDoubleColon Unicode   -> "∷"
  TokEquals                -> "="
  TokPipe                  -> "|"
  TokTick                  -> "`"
  TokDot                   -> "."
  TokComma                 -> ","
  TokUnderscore            -> "_"
  TokLowerName name        -> name
  TokUpperName name        -> name
  TokSymbol sym            -> sym
  TokHole hole             -> "?" <> hole
  TokChar raw _            -> "'" <> raw <> "'"
  TokString raw _          -> "\"" <> raw <> "\""
  TokRawString raw         -> "\"\"\"" <> raw <> "\"\"\""
  TokInt raw _             -> raw
  TokNumber raw _          -> raw

printTokens :: LexerResult -> Text
printTokens (toks, trailingComments) =
  Text.concat (map pp toks)
    <> Text.concat (map ppTc trailingComments)
  where
  pp (TokenAnn leading trailing, tok) =
    Text.concat (map ppLc leading)
      <> printToken tok
      <> Text.concat (map ppTc trailing)

  ppLc = \case
    Comment raw -> raw
    Space n -> Text.replicate n " "
    Line LF -> "\n"
    Line CRLF -> "\r\n"

  ppTc = \case
    Comment raw -> raw
    Space n -> Text.replicate n " "
    Line _ -> ""


