module Language.PureScript.CST.Lexer where

import Prelude

import Control.Applicative ((<|>))
import Data.Char (isAscii, isSymbol)
import Data.DList (DList, snoc)
import qualified Data.DList as DList
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Void (Void, absurd)
import qualified Data.Text as Text
import Language.PureScript.CST.Layout
import Language.PureScript.CST.Monad
import Language.PureScript.CST.Positions
import Language.PureScript.CST.Types
import Text.Megaparsec (MonadParsec)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P

type Lexer e m = MonadParsec e Text m

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
  leadingP :: P.Parsec Void Text [Comment LineFeed]
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
        (_, Left err) -> do
          let range = SourceRange parserPos (applyDelta parserPos (0, 1))
          kerr state $ convertErr range parserStack err
  where
  munchP :: forall void. P.Parsec Void Text (Token, ([Comment void], [Comment LineFeed]))
  munchP =
    (,) <$> token <*> breakComments
        <|> (TokEof, ([], [])) <$ P.eof

  convertErr pos stk bundle =
    case NonEmpty.head $ P.bundleErrors bundle of
      P.TrivialError _ err alts -> do
        ParserError pos [] stk $ ErrLexeme (printErrorItem <$> err) (printErrorItem <$> Set.toList alts)
      P.FancyError _ _ ->
        ParserError pos [] stk $ ErrEof

  printErrorItem = \case
    P.Tokens cs -> NonEmpty.toList cs
    P.Label cs -> NonEmpty.toList cs
    P.EndOfInput -> "end of input"

space :: Lexer e m => m Int
space = Text.length <$> P.takeWhile1P (Just "space") (== ' ')

line :: Lexer e m => m LineFeed
line = CRLF <$ P.crlf <|> LF <$ P.newline

lineComment :: Lexer e m => m Text
lineComment = do
  pre <- P.chunk "--"
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
      , TokForall        Unicode <$ P.char '∀'
      , TokEquals                <$ P.char '='
      , TokPipe                  <$ P.char '|'
      , TokDot                   <$ P.char '.'
      , TokBackslash             <$ P.char '\\'
      ] <* P.notFollowedBy symbolChar

  , P.try $ TokForall ASCII <$ (P.string "forall" <* P.notFollowedBy identLetter)
  , P.try $ TokUnderscore <$ (P.char '_' <* P.notFollowedBy identLetter)
  , P.label "hole" $ P.try $ TokHole <$> (P.char '?' *> lname)
  , P.label "identifier" identifier
  , P.label "character" $ uncurry TokChar <$> charLiteral
  , P.try $ P.label "raw string" $ TokRawString <$> rawStringLiteral
  , P.label "string" $ uncurry TokString <$> stringLiteral
  , P.try $ P.label "number" $ uncurry TokNumber <$> numberLiteral
  , P.try $ P.label "hex" $ uncurry TokInt <$> hexLiteral
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
    (delimiter *> pure acc')
      <|> (go . (acc' <>) =<< P.takeP Nothing 3)

intLiteral :: Lexer e m => m (Text, Integer)
intLiteral = P.match P.decimal

hexLiteral :: Lexer e m => m (Text, Integer)
hexLiteral = P.match $ P.string "0x" *> P.hexadecimal

numberLiteral :: Lexer e m => m (Text, Double)
numberLiteral = P.match P.float
