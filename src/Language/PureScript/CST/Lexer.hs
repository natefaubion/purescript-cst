module Language.PureScript.CST.Lexer
  ( initialParserState
  , lex
  , recover
  , munch
  ) where

import Prelude hiding (lex)

import Control.Monad (join)
import qualified Data.Char as Char
import qualified Data.DList as DList
import Data.Foldable (foldl')
import Data.Ratio ((%))
import Data.Text (Text)
import qualified Data.Text as Text
import Language.PureScript.CST.Monad
import Language.PureScript.CST.Layout
import Language.PureScript.CST.Positions
import Language.PureScript.CST.Types

initialParserState :: Text -> ParserState
initialParserState src = do
  let (parserLeading, src') = comments src
  ParserState
    { parserBuff = []
    , parserPos = advanceLeading (SourcePos 1 1) parserLeading
    , parserLeading = parserLeading
    , parserSource = src'
    , parserStack = [(SourcePos 0 0, LytRoot)]
    , parserErrors = []
    }

lex :: Text -> Either [ParserError] [SourceToken]
lex src = runParser (initialParserState src) (go mempty)
  where
  go acc = do
    tok <- munch
    case snd tok of
      TokEof -> pure $ DList.toList acc
      _      -> go (acc `DList.snoc` tok)

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
      then go (acc `DList.snoc` tok)
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
      case tokenAndComments parserSource of
        (Right (TokEof, _), _) -> do
          let
            toks = unwindLayout parserPos parserStack
            state' = state
              { parserBuff = tail toks
              , parserStack = []
              }
          ksucc state' (head toks)
        (Right (tok, (trailing, parserLeading')), parserSource') -> do
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
              { parserSource = parserSource'
              , parserBuff = tail toks
              , parserPos = parserPos'
              , parserLeading = parserLeading'
              , parserStack = parserStack'
              }
          ksucc state' (head toks)
        (Left err, parserSource') -> do
          let
            len1 = Text.length parserSource
            len2 = Text.length parserSource'
            chunk = Text.take (max 0 (len1 - len2)) parserSource
            chunkDelta = textDelta chunk
            pos = applyDelta parserPos chunkDelta
          kerr state $ ParserError (SourceRange pos $ applyDelta pos (0, 1)) [] parserStack err

type Lexer = ParserM ParserErrorType Text

{-# INLINE next #-}
next :: Lexer Char
next = Parser $ \inp kerr ksucc ->
  case Text.uncons inp of
    Just (ch, inp') -> ksucc inp' ch
    Nothing -> kerr inp ErrEof

{-# INLINE maybeNext #-}
maybeNext :: Lexer (Maybe Char)
maybeNext = Parser $ \inp _ ksucc ->
  case Text.uncons inp of
    Just (ch, inp') -> ksucc inp' (Just ch)
    Nothing -> ksucc inp Nothing

{-# INLINE nextWhile #-}
nextWhile :: (Char -> Bool) -> Lexer Text
nextWhile p = Parser $ \inp _ ksucc -> do
  let (chs, inp') = Text.span p inp
  ksucc inp' chs

{-# INLINE peek #-}
peek :: Lexer (Maybe Char)
peek = Parser $ \inp _ ksucc -> do
  case Text.uncons inp of
    Just (ch, _) -> ksucc inp (Just ch)
    Nothing -> ksucc inp Nothing

{-# INLINE throw #-}
throw :: ParserErrorType -> Lexer a
throw err = Parser $ \inp kerr _ -> kerr inp err

tokenAndComments :: Text -> (Either ParserErrorType (Token, ([Comment void], [Comment LineFeed])), Text)
tokenAndComments = \src -> k src (\inp err -> (Left err, inp)) (\inp res -> (Right res, inp))
  where
  Parser k = (,) <$> token <*> breakComments

comments :: Text -> ([Comment LineFeed], Text)
comments = \src -> k src (\_ _ -> ([], src)) (\inp (a, b) -> (a <> b, inp))
  where
  Parser k = breakComments

breakComments :: Lexer ([Comment void], [Comment LineFeed])
breakComments = k0 []
  where
  k0 acc = do
    spaces <- nextWhile (== ' ')
    lines' <- nextWhile isLineFeed
    let
      acc'
        | Text.null spaces = acc
        | otherwise = Space (Text.length spaces) : acc
    if Text.null lines'
      then do
        mbComm <- comment
        case mbComm of
          Just comm -> k0 (comm : acc')
          Nothing   -> pure (reverse acc', [])
      else
        k1 acc' (goWs [] $ Text.unpack lines')

  k1 trl acc = do
    ws <- nextWhile (\c -> c == ' ' || isLineFeed c)
    let acc' = goWs acc $ Text.unpack ws
    mbComm <- comment
    case mbComm of
      Just comm -> k1 trl (comm : acc')
      Nothing   -> pure (reverse trl, reverse acc')

  goWs a ('\r' : '\n' : ls) = goWs (Line CRLF : a) ls
  goWs a ('\r' : ls) = goWs (Line CRLF : a) ls
  goWs a ('\n' : ls) = goWs (Line LF : a) ls
  goWs a (' ' : ls) = goSpace a 1 ls
  goWs a _ = a

  goSpace a !n (' ' : ls) = goSpace a (n + 1) ls
  goSpace a !n ls = goWs (Space n : a) ls

  isBlockComment = Parser $ \inp _ ksucc ->
    case Text.uncons inp of
      Just ('-', inp2) ->
        case Text.uncons inp2 of
          Just ('-', inp3) ->
            ksucc inp3 $ Just False
          _ ->
            ksucc inp Nothing
      Just ('{', inp2) ->
        case Text.uncons inp2 of
          Just ('-', inp3) ->
            ksucc inp3 $ Just True
          _ ->
            ksucc inp Nothing
      _ ->
        ksucc inp Nothing

  comment = isBlockComment >>= \case
    Just True  -> Just <$> blockComment "{-"
    Just False -> Just <$> lineComment "--"
    Nothing    -> pure $ Nothing

  lineComment acc = do
    comm <- nextWhile (\c -> c /= '\r' && c /= '\n')
    pure $ Comment (acc <> comm)

  blockComment acc = do
    chs <- nextWhile (/= '-')
    dashes <- nextWhile (== '-')
    if Text.null dashes
      then pure $ Comment $ acc <> chs
      else peek >>= \case
        Just '}' -> next *> pure (Comment $ acc <> chs <> dashes <> "}")
        _ -> blockComment (acc <> chs <> dashes)

token :: Lexer Token
token = maybeNext >>= maybe (pure TokEof) k0
  where
  k0 ch1 = case ch1 of
    '('  -> pure TokLeftParen
    ')'  -> pure TokRightParen
    '{'  -> pure TokLeftBrace
    '}'  -> pure TokRightBrace
    '['  -> pure TokLeftSquare
    ']'  -> pure TokRightSquare
    '`'  -> pure TokTick
    ','  -> pure TokComma
    '∷'  -> orSymbol1 (TokDoubleColon Unicode) ch1
    '←'  -> orSymbol1 (TokLeftArrow Unicode) ch1
    '→'  -> orSymbol1 (TokRightArrow Unicode) ch1
    '⇒'  -> orSymbol1 (TokRightFatArrow Unicode) ch1
    '∀'  -> orSymbol1 (TokForall Unicode) ch1
    '|'  -> orSymbol1 TokPipe ch1
    '.'  -> orSymbol1 TokDot ch1
    '\\' -> orSymbol1 TokBackslash ch1
    '<'  -> orSymbol2 (TokLeftArrow ASCII) ch1 '-'
    '-'  -> orSymbol2 (TokRightArrow ASCII) ch1 '>'
    '='  -> orSymbol2' TokEquals (TokRightFatArrow ASCII) ch1 '>'
    ':'  -> orSymbol2' (TokSymbol [] ":") (TokDoubleColon ASCII) ch1 ':'
    '?'  -> hole
    '\'' -> char
    '"'  -> string
    _  | Char.isDigit ch1 -> number ch1
       | Char.isUpper ch1 -> upper [] ch1
       | isIdentStart ch1 -> lower [] ch1
       | isSymbolChar ch1 -> symbol [] [ch1]
       | otherwise        -> throw $ ErrLexeme (Just [ch1]) []

  {-# INLINE orSymbol1 #-}
  orSymbol1 tok ch1 = join $ Parser $ \inp _ ksucc ->
    case Text.uncons inp of
      Just (ch2, inp2) | isSymbolChar ch2 ->
        ksucc inp2 $ symbol [] [ch1, ch2]
      _ ->
        ksucc inp $ pure tok

  {-# INLINE orSymbol2 #-}
  orSymbol2 tok ch1 ch2 = join $ Parser $ \inp _ ksucc ->
    case Text.uncons inp of
      Just (ch2', inp2) | ch2 == ch2' ->
        case Text.uncons inp2 of
          Just (ch3, inp3) | isSymbolChar ch3 ->
            ksucc inp3 $ symbol [] [ch1, ch2, ch3]
          _ ->
            ksucc inp2 $ pure tok
      _ ->
        ksucc inp $ symbol [] [ch1]

  {-# INLINE orSymbol2' #-}
  orSymbol2' tok1 tok2 ch1 ch2 = join $ Parser $ \inp _ ksucc ->
    case Text.uncons inp of
      Just (ch2', inp2) | ch2 == ch2' ->
        case Text.uncons inp2 of
          Just (ch3, inp3) | isSymbolChar ch3 ->
            ksucc inp3 $ symbol [] [ch1, ch2, ch3]
          _ ->
            ksucc inp2 $ pure tok2
      Just (ch2', inp2) | isSymbolChar ch2' ->
        ksucc inp2 $ symbol [] [ch1, ch2']
      _ ->
        ksucc inp $ pure tok1

  symbol qual pre = do
    rest <- nextWhile isSymbolChar
    pure . TokSymbol (reverse qual) $ Text.pack pre <> rest

  upper qual pre = do
    rest <- nextWhile isIdentChar
    ch1  <- peek
    let name = Text.cons pre rest
    case ch1 of
      Just '.' -> do
        let qual' = name : qual
        ch2 <- next *> next
        if | Char.isUpper ch2 -> upper qual' ch2
           | isIdentStart ch2 -> lower qual' ch2
           | isSymbolChar ch2 -> symbol qual' [ch2]
           | otherwise -> throw $ ErrLexeme (Just [ch2]) []
      _ ->
        pure $ TokUpperName (reverse qual) name

  lower qual pre = do
    rest <- nextWhile isIdentChar
    case pre of
      '_' | Text.null rest ->
        if null qual
          then pure TokUnderscore
          else throw $ ErrLexeme (Just [pre]) []
      _ ->
        case Text.cons pre rest of
          "forall" | null qual -> pure $ TokForall ASCII
          name -> pure $ TokLowerName (reverse qual) name

  hole = do
    name <- nextWhile isIdentChar
    if Text.null name
      then symbol [] ['?']
      else pure $ TokHole name

  char = do
    ch1 <- next
    (raw, ch) <- case ch1 of
      '\\' -> do
        (raw, ch2) <- escape
        pure (Text.cons '\\' raw, ch2)
      ch ->
        pure (Text.singleton ch, ch)
    ch2 <- next
    case ch2 of
      '\''
        | fromEnum ch > 0xFFFF -> throw ErrAstralCodePointInChar
        | otherwise -> pure $ TokChar raw ch
      _ ->
        throw $ ErrLexeme (Just [ch2]) []

  string = do
    quotes1 <- nextWhile (== '"')
    case Text.length quotes1 of
      0 -> do
        let
          go raw acc = do
            chs <- nextWhile isNormalStringChar
            ch  <- next
            let
              raw' = raw <> chs
              acc' = acc <> chs
            case ch of
              '"'  -> pure $ TokString raw' acc'
              '\\' -> goEscape (raw' <> Text.singleton ch) acc'
              _    -> throw ErrLineFeedInString

          goEscape raw acc = do
            mbCh <- peek
            case mbCh of
              Just ch1 | isStringGapChar ch1 -> do
                gap <- nextWhile isStringGapChar
                ch2 <- next
                case ch2 of
                  '"'  -> pure $ TokString (raw <> gap) acc
                  '\\' -> go (raw <> gap <> "\\") acc
                  _    -> throw ErrCharEscape -- TODO error
              _ -> do
                (raw', ch) <- escape
                go (raw <> raw') (acc <> Text.singleton ch)
        go "" ""
      1 ->
        pure $ TokString "" ""
      n | n >= 5 -> do
        let str = Text.take 5 quotes1
        pure $ TokString str str
      _ -> do
        let
          go acc = do
            chs <- nextWhile (/= '"')
            quotes2 <- nextWhile (== '"')
            case Text.length quotes2 of
              0          -> throw ErrEof
              n | n >= 3 -> pure $ TokRawString $ acc <> chs <> Text.drop 3 quotes2
              _          -> go (acc <> chs <> quotes2)
        go ""

  escape = do
    ch <- peek
    case ch of
      Just 't'  -> next *> pure ("\t", '\t')
      Just 'r'  -> next *> pure ("\\r", '\r')
      Just 'n'  -> next *> pure ("\\n", '\n')
      Just '"'  -> next *> pure ("\"", '"')
      Just '\'' -> next *> pure ("'", '\'')
      Just '\\' -> next *> pure ("\\", '\\')
      Just 'x'  -> (*>) next $ Parser $ \inp kerr ksucc -> do
        let
          go n acc (ch' : chs)
            | Char.isHexDigit ch' = go (n * 16 + Char.digitToInt ch') (ch' : acc) chs
          go n acc _
            | n <= 0x10FFFF =
                ksucc (Text.drop (length acc) inp)
                  (Text.pack $ reverse acc, Char.chr n)
            | otherwise =
                kerr inp ErrCharEscape -- TODO
        go 0 [] $ Text.unpack $ Text.take 6 inp
      _ -> throw ErrCharEscape

  number ch1 = do
    (raw, int) <- integer ch1
    fraction >>= \case
      Just (raw', frac) ->
        pure $ TokNumber (raw <> "." <> raw') (fromIntegral int + frac)
      Nothing ->
        pure $ TokInt raw int

  integer ch1 = do
    chs <- nextWhile isNumberChar
    case ch1 of
      '0' | Text.null chs -> do
        ch2 <- peek
        case ch2 of
          Just 'x' -> do
            chs' <- next *> nextWhile Char.isHexDigit
            let int = foldl' (\n c -> n * 10 + fromIntegral (Char.digitToInt c)) 0 $ Text.unpack chs
            pure ("0x" <> chs', int)
          _ ->
            pure ("0", 0)
      _ | Text.null chs ->
        pure (Text.singleton ch1, fromIntegral $ Char.digitToInt ch1)
      _ -> do
        let
          go n '_' = n
          go n c   = n * 10 + fromIntegral (Char.digitToInt c)
        pure (Text.singleton ch1 <> chs, foldl' go 0 $ ch1 : Text.unpack chs)

  fraction = do
    ch1 <- peek
    case ch1 of
      Just '.' -> do
        chs <- next *> nextWhile isNumberChar
        if Text.null chs
          then pure $ Just ("", 0)
          else do
            let
              go acc '_' = acc
              go (!plc, !n) c = (plc * 10, n + (fromIntegral (Char.digitToInt c) % plc))
              res = foldl' go (10, 0 % 1) $ Text.unpack chs
            pure $ Just (chs, fromRational $ snd res)
      _ ->
        pure Nothing

isSymbolChar :: Char -> Bool
isSymbolChar c = (c `elem` (":!#$%&*+./<=>?@\\^|-~" :: [Char])) || (not (Char.isAscii c) && Char.isSymbol c)

isIdentStart :: Char -> Bool
isIdentStart c = Char.isLower c || c == '_'

isIdentChar :: Char -> Bool
isIdentChar c = Char.isAlphaNum c || c == '_' || c == '\''

isNumberChar :: Char -> Bool
isNumberChar c = Char.isDigit c || c == '_'

isNormalStringChar :: Char -> Bool
isNormalStringChar c = c /= '"' && c /= '\\' && c /= '\r' && c /= '\n'

isStringGapChar :: Char -> Bool
isStringGapChar c = c == ' ' || c == '\r' || c == '\n'

isLineFeed :: Char -> Bool
isLineFeed c = c == '\r' || c == '\n'
