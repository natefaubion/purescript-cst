module Language.PureScript.CST.Monad where

import Prelude

import Data.List (sortBy)
import qualified Data.List.NonEmpty as NE
import Data.Ord (comparing)
import Data.Text (Text)
import Language.PureScript.CST.Errors
import Language.PureScript.CST.Layout
import Language.PureScript.CST.Positions
import Language.PureScript.CST.Types

type LexResult = Either (LexState, ParserError) SourceToken

data LexState = LexState
  { lexPos :: SourcePos
  , lexLeading :: [Comment LineFeed]
  , lexSource :: Text
  , lexStack :: LayoutStack
  } deriving (Show)

data ParserState = ParserState
  { parserBuff :: [LexResult]
  , parserErrors :: [ParserError]
  } deriving (Show)

newtype ParserM e s a =
  Parser (forall r. s -> (s -> e -> r) -> (s -> a -> r) -> r)

type Parser = ParserM ParserError ParserState

instance Functor (ParserM e s) where
  {-# INLINE fmap #-}
  fmap f (Parser k) =
    Parser $ \st kerr ksucc ->
      k st kerr (\st' a -> ksucc st' (f a))

instance Applicative (ParserM e s) where
  {-# INLINE pure #-}
  pure a = Parser $ \st _ k -> k st a
  {-# INLINE (<*>) #-}
  Parser k1 <*> Parser k2 =
    Parser $ \st kerr ksucc ->
      k1 st kerr $ \st' f ->
        k2 st' kerr $ \st'' a ->
          ksucc st'' (f a)

instance Monad (ParserM e s) where
  {-# INLINE return #-}
  return = pure
  {-# INLINE (>>=) #-}
  Parser k1 >>= k2 =
    Parser $ \st kerr ksucc ->
      k1 st kerr $ \st' a -> do
        let Parser k3 = k2 a
        k3 st' kerr ksucc

runParser :: ParserState -> Parser a -> (ParserState, Either (NE.NonEmpty ParserError) a)
runParser st (Parser k) = k st left right
  where
  left st'@(ParserState {..}) err =
    (st', Left $ NE.sortBy (comparing errRange) $ err NE.:| parserErrors)

  right st'@(ParserState {..}) res
    | null parserErrors = (st', Right res)
    | otherwise = (st', Left $ NE.fromList $ sortBy (comparing errRange) parserErrors)

{-# INLINE throw #-}
throw :: e -> ParserM e s a
throw e = Parser $ \st kerr _ -> kerr st e

parseError :: SourceToken -> Parser a
parseError tok = Parser $ \st kerr _ ->
  kerr st $ ParserError
    { errRange = tokRange . tokAnn $ tok
    , errToks = [tok]
    , errStack = [] -- TODO parserStack st
    , errType = ErrToken
    }

mkParserError :: LayoutStack -> [SourceToken] -> ParserErrorType -> ParserError
mkParserError stack toks ty =
  ParserError
    { errRange =  range
    , errToks = toks
    , errStack = stack
    , errType = ty
    }
  where
  range = case toks of
    [] -> SourceRange (SourcePos 0 0) (SourcePos 0 0)
    _  -> widen (tokRange . tokAnn $ head toks) (tokRange . tokAnn $ last toks)

addFailure :: [SourceToken] -> ParserErrorType -> Parser ()
addFailure toks ty = Parser $ \st _ ksucc ->
  ksucc (st { parserErrors = mkParserError [] toks ty : parserErrors st }) ()

addFailures :: [ParserError] -> Parser ()
addFailures errs = Parser $ \st _ ksucc ->
  ksucc (st { parserErrors = errs <> parserErrors st }) ()

parseFail' :: [SourceToken] -> ParserErrorType -> Parser a
parseFail' toks msg = Parser $ \st kerr _ -> kerr st (mkParserError [] toks msg)

parseFail :: SourceToken -> ParserErrorType -> Parser a
parseFail = parseFail' . pure

pushBack :: SourceToken -> Parser ()
pushBack tok = Parser $ \st _ ksucc ->
  ksucc (st { parserBuff = Right tok : parserBuff st }) ()

{-# INLINE tryPrefix #-}
tryPrefix :: Parser a -> Parser b -> Parser (Maybe a, b)
tryPrefix (Parser lhs) rhs = Parser $ \st kerr ksucc ->
  lhs st
    (\_ _ -> do
      let Parser k = (Nothing,) <$> rhs
      k st kerr ksucc)
    (\st' res -> do
      let Parser k = (Just res,) <$> rhs
      k st' kerr ksucc)
