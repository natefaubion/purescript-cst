module Language.PureScript.CST.Lexer where

import Prelude

import Control.Applicative ((<|>))
import Data.Char (isAscii, isSymbol)
import Data.DList (DList, snoc)
import qualified Data.DList as DList
import Data.Foldable (foldl')
import Data.Text (Text)
import Data.Void (Void, absurd)
import qualified Data.Text as Text
import Language.PureScript.CST.Types (Token(..), TokenAnn(..), Comment(..), LineFeed(..), SourcePos(..), SourceRange(..), SourceStyle(..), SourceToken)
import Text.Megaparsec (MonadParsec)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P

type Lexer e m = MonadParsec e Text m

type LexerResult = ([SourceToken], [Comment LineFeed])

data LexerState = LexerState
  { lexAcc :: DList SourceToken
  , lexLeading :: [Comment LineFeed]
  , lexTok :: Token
  , lexPos :: SourcePos
  , lexStack :: LayoutStack
  }

data LayoutDelim
  = LayoutParen
  | LayoutBrace
  | LayoutSquare
  | LayoutIndent
  deriving (Show, Eq)

type LayoutStack = [(Int, LayoutDelim)]

layoutToken :: SourcePos -> Token -> SourceToken
layoutToken pos = (ann,)
  where
  ann = TokenAnn
    { tokRange = SourceRange pos pos
    , tokLeadingComments = []
    , tokTrailingComments = []
    }

hasLayout :: Token -> Maybe LayoutDelim
hasLayout = \case
  TokLeftParen -> Just LayoutParen
  TokLeftBrace -> Just LayoutBrace
  TokLeftSquare -> Just LayoutSquare
  TokLowerName [] "let" -> Just LayoutIndent
  TokLowerName [] "where" -> Just LayoutIndent
  TokLowerName [] "do" -> Just LayoutIndent
  TokLowerName [] "of" -> Just LayoutIndent
  _ -> Nothing

closesLayout :: Token -> Maybe LayoutDelim
closesLayout = \case
  TokRightParen -> Just LayoutParen
  TokRightBrace -> Just LayoutBrace
  TokRightSquare -> Just LayoutSquare
  TokLowerName [] "in" -> Just LayoutIndent
  _ -> Nothing

tokens :: forall e m. Lexer e m => m LexerResult
tokens = do
  leading <- uncurry (<>) <$> breakComments
  mbTok <- P.optional token
  case mbTok of
    Nothing -> pure (mempty, leading)
    Just tok ->
      loop False $ LexerState
        { lexAcc = mempty
        , lexLeading = leading
        , lexTok = tok
        , lexPos = advanceLeading (SourcePos 1 1) leading
        , lexStack = [(0, LayoutIndent)]
        }
  where
  loop :: Bool -> LexerState -> m LexerResult
  loop lytStarted (LexerState {..}) = do
    (trailing, leading) <- breakComments
    let
      (endPos, lexPos') =
        ( advanceToken lexPos lexTok
        , advanceLeading (advanceTrailing endPos trailing) leading
        )

      tokAnn = TokenAnn
        { tokRange = SourceRange lexPos endPos
        , tokLeadingComments = lexLeading
        , tokTrailingComments = trailing
        }

      mbLytStart = hasLayout lexTok
      tokCol = srcColumn lexPos
      nextTokCol = srcColumn lexPos'

      k0 :: m LexerResult
      k0 = case head lexStack of
        (indCol, LayoutIndent) | tokCol < indCol ->
          uncurry k1 $ collapse lexPos lexStack lexAcc
        _ -> k1 lexStack lexAcc

      k1 :: LayoutStack -> DList SourceToken -> m LexerResult
      k1 lexStack' lexAcc' = case (head lexStack', closesLayout lexTok) of
        ((indCol, LayoutIndent), Nothing)
          | Just LayoutIndent <- mbLytStart, nextTokCol == indCol ->
              k2 (tail lexStack') $ lexAcc' `snoc` layoutToken lexPos TokLayoutEnd
          | tokCol == indCol && not lytStarted ->
              k2 lexStack' $ lexAcc' `snoc` layoutToken lexPos TokLayoutSep
        ((indCol, LayoutIndent), Just LayoutIndent)
          | indCol > 0 ->
              k2 (tail lexStack') $ lexAcc' `snoc` layoutToken lexPos TokLayoutEnd
          | otherwise ->
              k2 lexStack' lexAcc'
        ((0, delim), Just delim') | delim' == delim ->
          k2 (tail lexStack') $ lexAcc'
        (_, Nothing) ->
          k2 lexStack' lexAcc'
        _ -> fail $ "Mismatched delimiters or indentation"

      k2 :: LayoutStack -> DList SourceToken -> m LexerResult
      k2 lexStack' lexAcc' =
        k3 lexStack' $ lexAcc' `snoc` (tokAnn, lexTok)

      k3 :: LayoutStack -> DList SourceToken -> m LexerResult
      k3 lexStack' lexAcc' = case mbLytStart of
        Just LayoutIndent ->
          k4 ((srcColumn lexPos', LayoutIndent) : lexStack') $
            lexAcc' `snoc` layoutToken lexPos' TokLayoutStart
        Just delim ->
          k4 ((0, delim) : lexStack') lexAcc'
        Nothing ->
          k4 lexStack' lexAcc'

      k4 :: LayoutStack -> DList SourceToken -> m LexerResult
      k4 lexStack' lexAcc' = P.optional token >>= \case
        Nothing -> do
          let result = DList.toList $ unwind lexPos' lexStack' lexAcc'
          pure (result, leading)
        Just lexTok' ->
          loop (mbLytStart == Just LayoutIndent) $
            LexerState lexAcc' leading lexTok' lexPos' lexStack'
    k0

  collapse
    :: SourcePos
    -> LayoutStack
    -> DList SourceToken
    -> (LayoutStack, DList SourceToken)
  collapse tokPos@(SourcePos _ tokCol) = go
    where
    go ((indCol, LayoutIndent) : stack) acc
      | tokCol < indCol = go stack $ acc `snoc` layoutToken tokPos TokLayoutEnd
    go stack acc = (stack, acc)

  unwind
    :: SourcePos
    -> LayoutStack
    -> DList SourceToken
    -> DList SourceToken
  unwind tokPos = go
    where
    go ((indCol, LayoutIndent) : stack) acc
      | indCol > 0 = go stack $ acc `snoc` layoutToken tokPos TokLayoutEnd
    go _ acc = acc

advanceToken :: SourcePos -> Token -> SourcePos
advanceToken pos = applyDelta pos . tokenDelta

advanceLeading :: SourcePos -> [Comment LineFeed] -> SourcePos
advanceLeading pos = foldl' (\a -> applyDelta a . commentDelta lineDelta) pos

advanceTrailing :: SourcePos -> [Comment Void] -> SourcePos
advanceTrailing pos = foldl' (\a -> applyDelta a . commentDelta (const (0, 0))) pos

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
breakComments = go (mempty :: DList (Comment Void))
  where
  go ts = do
    isLine <- True <$ P.lookAhead line <|> pure False
    if isLine
      then do
        ls <- P.many (Line <$> line <|> trailingComment)
        pure (fmap absurd <$> DList.toList ts, ls)
      else do
        mbT <- P.optional trailingComment
        case mbT of
          Nothing -> pure ([], fmap absurd <$> DList.toList ts)
          Just t -> go (ts `snoc` t)

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
      , TokRightFatArrow ASCII   <$ P.string "=>"
      , TokRightFatArrow Unicode <$ P.char '⇒'
      , TokEquals                <$ P.char '='
      , TokPipe                  <$ P.char '|'
      , TokDot                   <$ P.char '.'
      ] <* P.notFollowedBy symbolChar

  , P.try $ TokUnderscore <$ (P.char '_' <* P.notFollowedBy identLetter)
  , P.label "hole" $ P.try $ TokHole <$> (P.char '?' *> lname)
  , P.label "identifier" identifier
  , P.label "character" $ uncurry TokChar <$> charLiteral
  , P.label "raw string" $ TokRawString <$> rawStringLiteral
  , P.label "string" $ uncurry TokString <$> stringLiteral
  , P.try $ P.label "number" $ uncurry TokNumber <$> numberLiteral
  , P.label "integer" $ uncurry TokInt <$> intLiteral
  ]

identifier :: Lexer e m => m Token
identifier = go []
  where
  go accum =
    (do
      ident <- P.try uname
      (P.char '.' *> go (ident : accum))
        <|> pure (TokUpperName (reverse accum) ident))
    <|> go1 (reverse accum)

  go1 qual =
    TokLowerName qual <$> lname
      <|> TokSymbol qual <$> symbol

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
  TokRightFatArrow ASCII   -> (0, 2)
  TokRightFatArrow Unicode -> (0, 1)
  TokDoubleColon ASCII     -> (0, 2)
  TokDoubleColon Unicode   -> (0, 1)
  TokEquals                -> (0, 1)
  TokPipe                  -> (0, 1)
  TokTick                  -> (0, 1)
  TokDot                   -> (0, 1)
  TokComma                 -> (0, 1)
  TokUnderscore            -> (0, 1)
  TokLowerName qual name   -> (0, qualDelta qual + Text.length name)
  TokUpperName qual name   -> (0, qualDelta qual + Text.length name)
  TokSymbol qual sym       -> (0, qualDelta qual + Text.length sym)
  TokHole hole             -> (0, Text.length hole + 1)
  TokChar raw _            -> (0, Text.length raw + 2)
  TokInt raw _             -> (0, Text.length raw)
  TokNumber raw _          -> (0, Text.length raw)
  TokString raw _          -> multiLine 1 $ textDelta raw
  TokRawString raw         -> multiLine 3 $ textDelta raw
  TokLayoutStart           -> (0, 0)
  TokLayoutSep             -> (0, 0)
  TokLayoutEnd             -> (0, 0)

qualDelta :: [Text] -> Int
qualDelta = foldr ((+) . (+ 1) . Text.length) 0

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
  TokRightFatArrow ASCII   -> "=>"
  TokRightFatArrow Unicode -> "⇒"
  TokDoubleColon ASCII     -> "::"
  TokDoubleColon Unicode   -> "∷"
  TokEquals                -> "="
  TokPipe                  -> "|"
  TokTick                  -> "`"
  TokDot                   -> "."
  TokComma                 -> ","
  TokUnderscore            -> "_"
  TokLowerName qual name   -> printQual qual <> name
  TokUpperName qual name   -> printQual qual <> name
  TokSymbol qual sym       -> printQual qual <> sym
  TokHole hole             -> "?" <> hole
  TokChar raw _            -> "'" <> raw <> "'"
  TokString raw _          -> "\"" <> raw <> "\""
  TokRawString raw         -> "\"\"\"" <> raw <> "\"\"\""
  TokInt raw _             -> raw
  TokNumber raw _          -> raw
  TokLayoutStart           -> ""
  TokLayoutSep             -> ""
  TokLayoutEnd             -> ""

printQual :: [Text] -> Text
printQual = Text.concat . map (<> ".")

printTokens :: LexerResult -> Text
printTokens (toks, trailingComments) =
  Text.concat (map pp toks)
    <> Text.concat (map ppTc trailingComments)
  where
  pp (TokenAnn _ leading trailing, tok) =
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


