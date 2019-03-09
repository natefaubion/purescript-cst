module Language.PureScript.CST.Lexer where

import Prelude

import Control.Applicative ((<|>))
import Data.Char (isAscii, isSymbol, isHexDigit, digitToInt, chr)
import Data.DList (DList, snoc)
import qualified Data.DList as DList
import qualified Data.List.NonEmpty as NonEmpty
import Data.Functor (($>))
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Void (Void, absurd)
import qualified Data.Text as Text
import Language.PureScript.CST.Layout
import Language.PureScript.CST.Monad
import Language.PureScript.CST.Positions
import Language.PureScript.CST.Types
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P

type Lexer = P.Parsec ParserErrorType Text

initialParserState :: Text -> ParserState
initialParserState src =
  case P.runParser' leadingP initialState of
    (parserNext, Right parserLeading) ->
      ParserState
        { parserNext = parserNext
        , parserBuff = []
        , parserPos = advanceLeading (SourcePos 1 1) parserLeading
        , parserLeading = parserLeading
        , parserStack = [(SourcePos 0 0, LytRoot)]
        , parserErrors = []
        }
    (parserNext, _) ->
      ParserState
        { parserNext = parserNext
        , parserBuff = []
        , parserPos = SourcePos 1 1
        , parserLeading = []
        , parserStack = [(SourcePos 0 0, LytRoot)]
        , parserErrors = []
        }
  where
  leadingP :: P.Parsec ParserErrorType Text [Comment LineFeed]
  leadingP = uncurry (<>) <$> breakComments

  initialState :: P.State Text
  initialState = P.State
    { stateInput  = src
    , stateOffset = 0
    , statePosState = P.PosState
        { pstateInput = src
        , pstateOffset = 0
        , pstateSourcePos = P.initialPos "<file>"
        , pstateTabWidth = P.defaultTabWidth
        , pstateLinePrefix = ""
        }
    }

lex :: Text -> Either [ParserError] [SourceToken]
lex src = runParser (initialParserState src) (go mempty)
  where
  go acc = do
    tok <- munch
    case snd tok of
      TokEof -> pure $ DList.toList acc
      _      -> go (acc `snoc` tok)

recover :: ParserErrorType -> ([SourceToken] -> a) -> (SourceToken -> Parser a)
recover err k tok = do
  let revert = pushBack tok *> pure [tok]
  stk  <- getLayoutStack
  toks <-
    case snd tok of
      TokRightParen           -> revert
      TokRightBrace           -> revert
      TokRightSquare          -> revert
      TokLowerName [] "in"    -> pure [tok]
      TokLowerName [] "where" -> pure [tok]
      _ | null stk -> revert
        | otherwise -> do
            let
              p = const True
              -- p = case snd $ head stk of
              --   LytRoot      -> \(_, t) -> t == TokEof
              --   LytParen     -> \(_, t) -> t == TokRightParen
              --   LytBrace     -> \(_, t) -> t == TokRightBrace
              --   LytSquare    -> \(_, t) -> t == TokRightSquare
              --   LytCase      -> \(_, t) -> t == TokLowerName [] "of"
              --   LytTick      -> \(_, t) -> t == TokTick
              --   LytLambda    -> \(_, t) -> t == TokRightArrow ASCII || t == TokRightArrow Unicode
              --   LytCaseGuard -> \(_, t) -> t == TokPipe || t == TokRightArrow ASCII || t == TokRightArrow Unicode || t == TokLayoutSep || t == TokLayoutEnd
              --   LytDeclGuard -> \(_, t) -> t == TokPipe || t == TokEquals || t == TokLayoutSep || t == TokLayoutEnd
              --   LytDecl      -> \(_, t) -> t == TokLayoutSep || t == TokLayoutEnd
              --   LytDeclWhere -> \(_, t) -> t == TokLayoutSep || t == TokLayoutEnd || t == TokLowerName [] "where"
              --   LytIndent _  -> \(_, t) -> t == TokLayoutSep || t == TokLayoutEnd
            (tok :) <$> munchWhile (not . p)
  addFailure toks err
  pure $ k toks

munchWhile :: (SourceToken -> Bool) -> Parser [SourceToken]
munchWhile p = go mempty
  where
  go acc = do
    tok <- munch
    if p tok
      then go (acc `snoc` tok)
      else do
        pushBack tok
        pure $ DList.toList acc

munch :: Parser SourceToken
munch = Parser $ \state@(ParserState {..}) kerr ksucc ->
  case parserBuff of
    tok : parserBuff' -> do
      let state' = state { parserBuff = parserBuff' }
      ksucc state' tok
    _ ->
      case P.runParser' munchP parserNext of
        (parserNext', Right (TokEof, _)) -> do
          let
            toks = unwindLayout parserPos parserStack
            state' = state
              { parserNext = parserNext'
              , parserBuff = tail toks
              , parserStack = []
              }
          ksucc state' (head toks)
        (parserNext', Right (tok, (trailing, parserLeading'))) -> do
          let
            endPos = advanceToken parserPos tok
            parserPos' = advanceLeading (advanceTrailing endPos trailing) parserLeading'
            tokenAnn = TokenAnn
              { tokRange = SourceRange parserPos endPos
              , tokLeadingComments = parserLeading
              , tokTrailingComments = trailing
              }
            (parserStack', toks) =
              insertLayout (tokenAnn, tok) parserPos' parserStack
            state' = state
              { parserNext = parserNext'
              , parserBuff = tail toks
              , parserPos = parserPos'
              , parserLeading = parserLeading'
              , parserStack = parserStack'
              }
          ksucc state' (head toks)
        (_, Left bundle) ->
          kerr state
            . convertErr parserStack
            . fst
            . P.attachSourcePos P.errorOffset (P.bundleErrors bundle)
            $ P.bundlePosState bundle
  where
  munchP :: forall void. P.Parsec ParserErrorType Text (Token, ([Comment void], [Comment LineFeed]))
  munchP =
    (,) <$> token <*> breakComments
        <|> (TokEof, ([], [])) <$ P.eof

  convertErr stk errs =
    case NonEmpty.head errs of
      (P.TrivialError _ err alts, pos) -> do
        let alts' = convertAlts alts
        ParserError (convertPos pos) [] stk $ maybe (ErrLexeme Nothing alts') (convertTrivialError alts') err
      (P.FancyError _ err, pos) ->
        ParserError (convertPos pos) [] stk . convertFancyError . head $ Set.toList err

  convertFancyError = \case
    P.ErrorFail err -> ErrLexeme (Just err) []
    P.ErrorIndentation _ _ _ -> ErrLexeme (Just "indentation") []
    P.ErrorCustom err -> err

  convertPos pos = do
    let pos' = SourcePos (P.unPos $ P.sourceLine pos) (P.unPos $ P.sourceColumn pos)
    SourceRange pos' $ applyDelta pos' (0, 1)

  convertTrivialError alts = \case
    P.Tokens cs -> ErrLexeme (Just $ show $ NonEmpty.toList cs) alts
    P.Label cs -> ErrLexeme (Just $ NonEmpty.toList cs) []
    P.EndOfInput -> ErrEof

  convertAlts =
    map printErrorItem . Set.toList

  printErrorItem = \case
    P.Tokens cs -> show $ NonEmpty.toList cs
    P.Label cs -> NonEmpty.toList cs
    P.EndOfInput -> "end of input"

space :: Lexer Int
space = Text.length <$> P.takeWhile1P (Just "space") (== ' ')

line :: Lexer LineFeed
line = CRLF <$ P.crlf <|> LF <$ P.newline

lineComment :: Lexer Text
lineComment = do
  chs <- P.string "--"
  (chs <>) <$> P.takeWhileP Nothing (\c -> c /= '\r' && c /= '\n')

blockComment :: Lexer Text
blockComment = P.string "{-" >>= go
  where
  go acc = do
    chs <- P.takeWhileP Nothing (/= '-')
    dashes <- P.takeWhileP Nothing (== '-')
    end <- P.optional (P.char '}')
    let acc' = acc <> chs <> dashes
    case end of
      Just end'
        | not (Text.null dashes) ->
            pure $ acc' <> Text.singleton end'
        | otherwise ->
            go $ acc' <> Text.singleton end'
      Nothing -> do
        isEof <- P.atEnd
        if isEof
          then pure acc'
          else go acc'

trailingComment :: Lexer (Comment void)
trailingComment =
  Space <$> space
    <|> Comment <$> (lineComment <|> blockComment)

breakComments :: Lexer ([Comment void], [Comment LineFeed])
breakComments = go (mempty :: DList (Comment Void))
  where
  go ts = do
    ln <- P.optional line
    case ln of
      Just ln' -> do
        ls <- P.many (Line <$> line <|> trailingComment)
        pure (fmap absurd <$> DList.toList ts, Line ln' : ls)
      Nothing -> do
        cs <- P.many trailingComment
        case cs of
          [] -> pure ([], fmap absurd <$> DList.toList ts)
          _  -> go (ts <> DList.fromList cs)

token :: Lexer Token
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
      , TokForall        Unicode <$ P.char '∀'
      , TokEquals                <$ P.char '='
      , TokPipe                  <$ P.char '|'
      , TokDot                   <$ P.char '.'
      , TokBackslash             <$ P.char '\\'
      ] <* P.notFollowedBy symbolChar

  , P.try $ TokForall ASCII <$ (P.string "forall" <* P.notFollowedBy identLetter)
  , P.try $ TokUnderscore <$ (P.char '_' <* P.notFollowedBy identLetter)
  , P.try $ TokHole <$> (P.char '?' *> lname)
  , identifier
  , uncurry TokChar <$> charLiteral
  , TokRawString <$> rawStringLiteral
  , uncurry TokString <$> stringLiteral
  , P.try $ uncurry TokNumber <$> numberLiteral
  , P.try $ uncurry TokInt <$> hexLiteral
  , uncurry TokInt <$> intLiteral
  ]

identifier :: Lexer Token
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

lname :: Lexer Text
lname = Text.cons <$> identStart <*> (Text.pack <$> P.many identLetter)

uname :: Lexer Text
uname = Text.cons <$> P.upperChar <*> (Text.pack <$> P.many identLetter)

symbol :: Lexer Text
symbol = P.takeWhile1P (Just "symbol") isSymbolChar

symbolChar :: Lexer Char
symbolChar = P.satisfy isSymbolChar

isSymbolChar :: Char -> Bool
isSymbolChar c = (c `elem` (":!#$%&*+./<=>?@\\^|-~" :: [Char])) || (not (isAscii c) && isSymbol c)

identStart :: Lexer Char
identStart = P.lowerChar <|> P.char '_'

identLetter :: Lexer Char
identLetter = P.alphaNumChar <|> P.char '_' <|> P.char '\''

validModuleName :: Text -> Bool
validModuleName s = '_' `notElemText` s

validUName :: Text -> Bool
validUName s = '\'' `notElemText` s

notElemText :: Char -> Text -> Bool
notElemText c = not . Text.any (== c)

charLiteral :: Lexer (Text, Char)
charLiteral = do
  (raw, ch) <- P.char '\'' *> P.match (charUnit (/= '\'')) <* P.char '\''
  if fromEnum ch > 0xFFFF
    then P.customFailure ErrAstralCodePointInChar
    else pure (raw, ch)

charUnit :: (Char -> Bool) -> Lexer Char
charUnit p = do
  ch <- P.satisfy p
  case ch of
    '\\' -> charEscape
    _    -> pure ch

charEscape :: Lexer Char
charEscape = do
  esc <- P.lookAhead P.anySingle
  case esc of
    't'  -> P.takeP Nothing 1 $> '\t'
    'r'  -> P.takeP Nothing 1 $> '\r'
    'n'  -> P.takeP Nothing 1 $> '\n'
    '"'  -> P.takeP Nothing 1 $> '"'
    '\'' -> P.takeP Nothing 1 $> '\''
    '\\' -> P.takeP Nothing 1 $> '\\'
    'x'  -> P.takeP Nothing 1 *> parseHex 0 0
    _    -> P.customFailure ErrCharEscape
  where
  parseHex :: Int -> Int -> Lexer Char
  parseHex n acc
    | n >= 4 = pure $ chr acc
    | otherwise = do
        ch <- P.optional $ P.satisfy isHexDigit
        case ch of
          Nothing  -> pure $ chr acc
          Just ch' -> parseHex (n + 1) (acc * 16 + digitToInt ch')

stringLiteral :: Lexer (Text, Text)
stringLiteral = P.char '"' *> go "" ""
  where
  go raw acc = do
    chs <- P.takeWhileP Nothing isNormalChar
    ch  <- P.anySingle
    let
      raw' = raw <> chs
      acc' = acc <> chs
    case ch of
      '"'  -> pure (raw', acc')
      '\\' -> goEscape (raw' <> Text.singleton ch) acc'
      _    -> P.customFailure ErrLineFeedInString

  goEscape raw acc = do
    lk <- P.lookAhead P.anySingle
    if isGapChar lk
      then do
        chs <- P.takeWhile1P Nothing isGapChar
        ch  <- P.char '\\' <|> P.char '"'
        case ch of
          '"' -> pure (raw <> chs, acc)
          _   -> go (raw <> chs <> Text.singleton ch) acc
      else do
        (raw', esc) <- P.match charEscape
        go (raw <> raw') (acc <> Text.singleton esc)

  isNormalChar c =
    c /= '"' && c /= '\\' && c /= '\r' && c /= '\n'

  isGapChar c =
    c == ' ' || c == '\r' || c == '\n'

rawStringLiteral :: Lexer Text
rawStringLiteral = P.string "\"\"\"" *> go mempty
  where
  go acc = do
    chs <- P.takeWhileP Nothing (/= '"')
    quotes <- P.takeWhile1P Nothing (== '"')
    if Text.length quotes >= 3
      then pure $ acc <> chs <> Text.dropEnd 3 quotes
      else go $ acc <> chs <> quotes

intLiteral :: Lexer (Text, Integer)
intLiteral = P.match P.decimal

hexLiteral :: Lexer (Text, Integer)
hexLiteral = P.match $ P.string "0x" *> P.hexadecimal

numberLiteral :: Lexer (Text, Double)
numberLiteral = P.match P.float
