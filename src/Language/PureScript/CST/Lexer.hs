module Language.PureScript.CST.Lexer
  ( initialParserState
  , lex
  , recover
  , munch
  ) where

import Prelude hiding (lex, exp, exponent, lines)

import Control.Monad (join)
import qualified Data.Char as Char
import qualified Data.DList as DList
import Data.Foldable (foldl')
import Data.Functor (($>))
import qualified Data.Scientific as Sci
import Data.Text (Text)
import qualified Data.Text as Text
import Language.PureScript.CST.Errors
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
    case tokValue tok of
      TokEof -> pure $ DList.toList acc
      _      -> go (acc `DList.snoc` tok)

recover :: ParserErrorType -> ([SourceToken] -> a) -> (SourceToken -> Parser a)
recover err k tok = do
  let revert = pushBack tok *> pure [tok]
  stk  <- getLayoutStack
  toks <-
    case tokValue tok of
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
    _ -> do
      let
        onSuccess _ (TokEof, _) = do
          let
            toks = unwindLayout parserPos parserStack
            state' = state
              { parserBuff = tail toks
              , parserStack = []
              }
          ksucc state' (head toks)
        onSuccess parserSource' (tok, (trailing, parserLeading')) = do
          let
            endPos = advanceToken parserPos tok
            parserPos' = advanceLeading (advanceTrailing endPos trailing) parserLeading'
            tokenAnn = TokenAnn
              { tokRange = SourceRange parserPos endPos
              , tokLeadingComments = parserLeading
              , tokTrailingComments = trailing
              }
            (parserStack', toks) =
              insertLayout (SourceToken tokenAnn tok) parserPos' parserStack
            state' = state
              { parserSource = parserSource'
              , parserBuff = tail toks
              , parserPos = parserPos'
              , parserLeading = parserLeading'
              , parserStack = parserStack'
              }
          ksucc state' (head toks)
        onError parserSource' err = do
          let
            len1 = Text.length parserSource
            len2 = Text.length parserSource'
            chunk = Text.take (max 0 (len1 - len2)) parserSource
            chunkDelta = textDelta chunk
            pos = applyDelta parserPos chunkDelta
          kerr state $ ParserError (SourceRange pos $ applyDelta pos (0, 1)) [] parserStack err
        Parser k = tokenAndComments
      k parserSource onError onSuccess

type Lexer = ParserM ParserErrorType Text

{-# INLINE next #-}
next :: Lexer ()
next = Parser $ \inp _ ksucc ->
  ksucc (Text.drop 1 inp) ()

{-# INLINE nextWhile #-}
nextWhile :: (Char -> Bool) -> Lexer Text
nextWhile p = Parser $ \inp _ ksucc -> do
  let (chs, inp') = Text.span p inp
  ksucc inp' chs

{-# INLINE peek #-}
peek :: Lexer (Maybe Char)
peek = Parser $ \inp _ ksucc ->
  if Text.null inp
    then ksucc inp Nothing
    else ksucc inp $ Just $ Text.head inp

{-# INLINE throw #-}
throw :: ParserErrorType -> Lexer a
throw err = Parser $ \inp kerr _ -> kerr inp err

{-# INLINE restore #-}
restore :: (ParserErrorType -> Bool) -> Lexer a -> Lexer a
restore p (Parser k) = Parser $ \inp kerr ksucc ->
  k inp (\inp' err -> kerr (if p err then inp else inp') err) ksucc

tokenAndComments :: Lexer (Token, ([Comment void], [Comment LineFeed]))
tokenAndComments = (,) <$> token <*> breakComments

comments :: Text -> ([Comment LineFeed], Text)
comments = \src -> k src (\_ _ -> ([], src)) (\inp (a, b) -> (a <> b, inp))
  where
  Parser k = breakComments

breakComments :: Lexer ([Comment void], [Comment LineFeed])
breakComments = k0 []
  where
  k0 acc = do
    spaces <- nextWhile (== ' ')
    lines  <- nextWhile isLineFeed
    let
      acc'
        | Text.null spaces = acc
        | otherwise = Space (Text.length spaces) : acc
    if Text.null lines
      then do
        mbComm <- comment
        case mbComm of
          Just comm -> k0 (comm : acc')
          Nothing   -> pure (reverse acc', [])
      else
        k1 acc' (goWs [] $ Text.unpack lines)

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
        Just '}' -> next $> Comment (acc <> chs <> dashes <> "}")
        _ -> blockComment (acc <> chs <> dashes)

token :: Lexer Token
token = peek >>= maybe (pure TokEof) k0
  where
  k0 ch1 = case ch1 of
    '('  -> next *> leftParen
    ')'  -> next $> TokRightParen
    '{'  -> next $> TokLeftBrace
    '}'  -> next $> TokRightBrace
    '['  -> next $> TokLeftSquare
    ']'  -> next $> TokRightSquare
    '`'  -> next $> TokTick
    ','  -> next $> TokComma
    '∷'  -> next *> orSymbol1 (TokDoubleColon Unicode) ch1
    '←'  -> next *> orSymbol1 (TokLeftArrow Unicode) ch1
    '→'  -> next *> orSymbol1 (TokRightArrow Unicode) ch1
    '⇒'  -> next *> orSymbol1 (TokRightFatArrow Unicode) ch1
    '∀'  -> next *> orSymbol1 (TokForall Unicode) ch1
    '|'  -> next *> orSymbol1 TokPipe ch1
    '.'  -> next *> orSymbol1 TokDot ch1
    '\\' -> next *> orSymbol1 TokBackslash ch1
    '<'  -> next *> orSymbol2 (TokLeftArrow ASCII) ch1 '-'
    '-'  -> next *> orSymbol2 (TokRightArrow ASCII) ch1 '>'
    '='  -> next *> orSymbol2' TokEquals (TokRightFatArrow ASCII) ch1 '>'
    ':'  -> next *> orSymbol2' (TokSymbol [] ":") (TokDoubleColon ASCII) ch1 ':'
    '?'  -> next *> hole
    '\'' -> next *> char
    '"'  -> next *> string
    _  | Char.isDigit ch1 -> restore (\err -> err == ErrNumberOutOfRange) (next *> number ch1)
       | Char.isUpper ch1 -> next *> upper [] ch1
       | isIdentStart ch1 -> next *> lower [] ch1
       | isSymbolChar ch1 -> next *> symbol [] [ch1]
       | otherwise        -> throw $ ErrLexeme (Just [ch1]) []

  {-# INLINE orSymbol1 #-}
  orSymbol1 :: Token -> Char -> Lexer Token
  orSymbol1 tok ch1 = join $ Parser $ \inp _ ksucc ->
    case Text.uncons inp of
      Just (ch2, inp2) | isSymbolChar ch2 ->
        ksucc inp2 $ symbol [] [ch1, ch2]
      _ ->
        ksucc inp $ pure tok

  {-# INLINE orSymbol2 #-}
  orSymbol2 :: Token -> Char -> Char -> Lexer Token
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
  orSymbol2' :: Token -> Token -> Char -> Char -> Lexer Token
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

  leftParen :: Lexer Token
  leftParen = Parser $ \inp kerr ksucc ->
    case Text.span isSymbolChar inp of
      (chs, inp2)
        | Text.null chs -> ksucc inp TokLeftParen
        | otherwise ->
            case Text.uncons inp2 of
              Just (')', inp3) ->
                case chs of
                  "→"  -> ksucc inp3 $ TokSymbolArr Unicode
                  "->" -> ksucc inp3 $ TokSymbolArr ASCII
                  _ | isReservedSymbol chs -> kerr inp $ ErrReservedSymbol chs
                    | otherwise -> ksucc inp3 $ TokSymbolName [] chs
              _ -> ksucc inp TokLeftParen

  symbolParen :: [Text] -> Lexer Token
  symbolParen qual = restore isReservedSymbolError $ peek >>= \case
    Just ch | isSymbolChar ch ->
      nextWhile isSymbolChar >>= \chs ->
        peek >>= \case
          Just ')'
            | isReservedSymbol chs -> throw $ ErrReservedSymbol chs
            | otherwise -> next $> TokSymbolName qual chs
          Just ch2 -> throw $ ErrLexeme (Just [ch2]) []
          Nothing  -> throw ErrEof
    Just ch -> throw $ ErrLexeme (Just [ch]) []
    Nothing -> throw ErrEof

  symbol :: [Text] -> [Char] -> Lexer Token
  symbol qual pre = do
    rest <- nextWhile isSymbolChar
    pure . TokSymbol (reverse qual) $ Text.pack pre <> rest

  upper :: [Text] -> Char -> Lexer Token
  upper qual pre = do
    rest <- nextWhile isIdentChar
    ch1  <- peek
    let name = Text.cons pre rest
    case ch1 of
      Just '.' -> do
        let qual' = name : qual
        next *> peek >>= \case
          Just '(' -> next *> symbolParen qual'
          Just ch2
            | Char.isUpper ch2 -> next *> upper qual' ch2
            | isIdentStart ch2 -> next *> lower qual' ch2
            | isSymbolChar ch2 -> next *> symbol qual' [ch2]
            | otherwise -> throw $ ErrLexeme (Just [ch2]) []
          Nothing ->
            throw ErrEof
      _ ->
        pure $ TokUpperName (reverse qual) name

  lower :: [Text] -> Char -> Lexer Token
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

  hole :: Lexer Token
  hole = do
    name <- nextWhile isIdentChar
    if Text.null name
      then symbol [] ['?']
      else pure $ TokHole name

  char :: Lexer Token
  char = do
    (raw, ch) <- peek >>= \case
      Just '\\' -> do
        (raw, ch2) <- next *> escape
        pure (Text.cons '\\' raw, ch2)
      Just ch ->
        next $> (Text.singleton ch, ch)
      Nothing ->
        throw $ ErrEof
    peek >>= \case
      Just '\''
        | fromEnum ch > 0xFFFF -> throw ErrAstralCodePointInChar
        | otherwise -> next $> TokChar raw ch
      Just ch2 ->
        throw $ ErrLexeme (Just [ch2]) []
      _ ->
        throw $ ErrEof

  string :: Lexer Token
  string = do
    quotes1 <- nextWhile (== '"')
    case Text.length quotes1 of
      0 -> do
        let
          go raw acc = do
            chs <- nextWhile isNormalStringChar
            let
              raw' = raw <> chs
              acc' = acc <> chs
            peek >>= \case
              Just '"'  -> next $> TokString raw' acc'
              Just '\\' -> next *> goEscape (raw' <> "\\") acc'
              Just _    -> throw ErrLineFeedInString
              Nothing   -> throw ErrEof

          goEscape raw acc = do
            mbCh <- peek
            case mbCh of
              Just ch1 | isStringGapChar ch1 -> do
                gap <- nextWhile isStringGapChar
                peek >>= \case
                  Just '"'  -> next $> TokString (raw <> gap) acc
                  Just '\\' -> next *> go (raw <> gap <> "\\") acc
                  Just ch   -> throw $ ErrCharInGap ch
                  Nothing   -> throw ErrEof
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

  escape :: Lexer (Text, Char)
  escape = do
    ch <- peek
    case ch of
      Just 't'  -> next $> ("\t", '\t')
      Just 'r'  -> next $> ("\\r", '\r')
      Just 'n'  -> next $> ("\\n", '\n')
      Just '"'  -> next $> ("\"", '"')
      Just '\'' -> next $> ("'", '\'')
      Just '\\' -> next $> ("\\", '\\')
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

  number :: Char -> Lexer Token
  number ch1 = peek >>= \ch2 -> case (ch1, ch2) of
    ('0', Just 'x') -> next *> hexadecimal
    (_, _) -> do
      mbInt <- integer1 ch1
      mbFraction <- fraction
      case (mbInt, mbFraction) of
        (Just (raw, int), Nothing) -> do
          let int' = digitsToInteger int
          exponent >>= \case
            Just (raw', exp) ->
              sciDouble (raw <> raw') $ Sci.scientific int' exp
            Nothing ->
              pure $ TokInt raw int'
        (Just (raw, int), Just (raw', frac)) -> do
          let sci = digitsToScientific int frac
          exponent >>= \case
            Just (raw'', exp) ->
              sciDouble (raw <> raw' <> raw'') $ uncurry Sci.scientific $ (+ exp) <$> sci
            Nothing ->
              sciDouble (raw <> raw') $ uncurry Sci.scientific sci
        (Nothing, Just (raw, frac)) -> do
          let sci = digitsToScientific [] frac
          exponent >>= \case
            Just (raw', exp) ->
              sciDouble (raw <> raw') $ uncurry Sci.scientific $ (+ exp) <$> sci
            Nothing ->
              sciDouble raw $ uncurry Sci.scientific sci
        (Nothing, Nothing) ->
          peek >>= \ch -> throw $ ErrLexeme (pure <$> ch) []

  sciDouble :: Text -> Sci.Scientific -> Lexer Token
  sciDouble raw sci = case Sci.toBoundedRealFloat sci of
    Left _ -> throw ErrNumberOutOfRange
    Right n -> pure $ TokNumber raw n

  integer :: Lexer (Maybe (Text, String))
  integer = peek >>= \case
    Just '0' -> next *> peek >>= \case
      Just ch | isNumberChar ch -> throw ErrLeadingZero
      _ -> pure $ Just ("0", "0")
    Just ch | isDigitChar ch -> Just <$> digits
    _ -> pure $ Nothing

  integer1 :: Char -> Lexer (Maybe (Text, String))
  integer1 = \case
    '0' -> peek >>= \case
      Just ch | isNumberChar ch -> throw ErrLeadingZero
      _ -> pure $ Just ("0", "0")
    ch | isDigitChar ch -> do
      (raw, chs) <- digits
      pure $ Just (Text.cons ch raw, ch : chs)
    _ -> pure $ Nothing

  fraction :: Lexer (Maybe (Text, String))
  fraction = peek >>= \case
    Just '.' -> do
      (raw, chs) <- next *> digits
      pure $ Just ("." <> raw, chs)
    _ -> pure $ Nothing

  digits :: Lexer (Text, String)
  digits = do
    raw <- nextWhile isDigitChar
    pure (raw, filter (/= '_') $ Text.unpack raw)

  exponent :: Lexer (Maybe (Text, Int))
  exponent = peek >>= \case
    Just 'e' -> do
      (neg, sign) <- next *> peek >>= \case
        Just '-' -> next $> (True, "-")
        Just '+' -> next $> (False, "+")
        _   -> pure (False, "")
      integer >>= \case
        Just (raw, chs) -> do
          let
            int | neg = negate $ digitsToInteger chs
                | otherwise = digitsToInteger chs
          pure $ Just ("e" <> sign <> raw, fromInteger int)
        Nothing -> throw ErrExpectedExponent
    _ ->
      pure Nothing

  hexadecimal :: Lexer Token
  hexadecimal = do
    chs <- nextWhile Char.isHexDigit
    pure $ TokInt ("0x" <> chs) $ digitsToIntegerBase 16 $ Text.unpack chs

digitsToInteger :: [Char] -> Integer
digitsToInteger = digitsToIntegerBase 10

digitsToIntegerBase :: Integer -> [Char] -> Integer
digitsToIntegerBase b = foldl' (\n c -> n * b + (toInteger (Char.digitToInt c))) 0

digitsToScientific :: [Char] -> [Char] -> (Integer, Int)
digitsToScientific = go 0 . reverse
  where
  go !exp is [] = (digitsToInteger (reverse is), exp)
  go !exp is (f : fs) = go (exp - 1) (f : is) fs

isSymbolChar :: Char -> Bool
isSymbolChar c = (c `elem` (":!#$%&*+./<=>?@\\^|-~" :: [Char])) || (not (Char.isAscii c) && Char.isSymbol c)

isReservedSymbolError :: ParserErrorType -> Bool
isReservedSymbolError = \case
  ErrReservedSymbol _ -> True
  _ -> False

isReservedSymbol :: Text -> Bool
isReservedSymbol = flip elem symbols
  where
  symbols =
    [ "::"
    , "∷"
    , "<-"
    , "←"
    , "->"
    , "→"
    , "=>"
    , "⇒"
    , "∀"
    , "|"
    , "."
    , "\\"
    , "="
    ]

isIdentStart :: Char -> Bool
isIdentStart c = Char.isLower c || c == '_'

isIdentChar :: Char -> Bool
isIdentChar c = Char.isAlphaNum c || c == '_' || c == '\''

isDigitChar :: Char -> Bool
isDigitChar c = c >= '0' && c <= '9'

isNumberChar :: Char -> Bool
isNumberChar c = c >= '0' && c <= '9' || c == '_'

isNormalStringChar :: Char -> Bool
isNormalStringChar c = c /= '"' && c /= '\\' && c /= '\r' && c /= '\n'

isStringGapChar :: Char -> Bool
isStringGapChar c = c == ' ' || c == '\r' || c == '\n'

isLineFeed :: Char -> Bool
isLineFeed c = c == '\r' || c == '\n'
