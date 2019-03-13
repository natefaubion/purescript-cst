module Language.PureScript.CST.Monad where

import Prelude

import Data.Text (Text)
import Language.PureScript.CST.Errors
import Language.PureScript.CST.Layout
import Language.PureScript.CST.Positions
import Language.PureScript.CST.Types

data ParserState = ParserState
  { parserBuff :: [SourceToken]
  , parserPos :: SourcePos
  , parserLeading :: [Comment LineFeed]
  , parserSource :: Text
  , parserStack :: LayoutStack
  , parserErrors :: [ParserError]
  }

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

runParser :: ParserState -> Parser a -> Either [ParserError] a
runParser st (Parser k) = k st left right
  where
  left (ParserState {..}) err =
    Left $ reverse $ err : parserErrors

  right (ParserState {..}) res
    | null parserErrors = Right res
    | otherwise = Left $ reverse parserErrors

parseError :: SourceToken -> Parser a
parseError tok = Parser $ \st kerr _ ->
  kerr st $ ParserError
    { errRange = tokRange . tokAnn $ tok
    , errToks = [tok]
    , errStack = parserStack st
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
  ksucc (st { parserErrors = mkParserError (parserStack st) toks ty : parserErrors st }) ()

parseFail' :: [SourceToken] -> ParserErrorType -> Parser a
parseFail' toks msg = Parser $ \st kerr _ -> kerr st (mkParserError (parserStack st) toks msg)

parseFail :: SourceToken -> ParserErrorType -> Parser a
parseFail = parseFail' . pure

getLeadingComments :: Parser [Comment LineFeed]
getLeadingComments = Parser $ \st _ ksucc -> ksucc st (parserLeading st)

getLayoutStack :: Parser LayoutStack
getLayoutStack = Parser $ \st _ ksucc -> ksucc st (parserStack st)

pushBack :: SourceToken -> Parser ()
pushBack tok = Parser $ \st _ ksucc -> ksucc (st { parserBuff = tok : parserBuff st }) ()
