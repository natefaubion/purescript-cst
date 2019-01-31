module Language.PureScript.CST.Monad where

import Prelude

import Data.Text (Text)
import Language.PureScript.CST.Layout
import Language.PureScript.CST.Types
import qualified Text.Megaparsec as P

data ParserError = ParserError
  { errPos :: SourcePos
  , errTok :: Maybe SourceToken
  , errAlts :: [String]
  , errMsg :: String
  }

data ParserState = ParserState
  { parserNext :: P.State Text
  , parserBuff :: [SourceToken]
  , parserPos :: SourcePos
  , parserLeading :: [Comment LineFeed]
  , parserStack :: LayoutStack
  }

newtype Parser a =
  Parser (forall r. ParserState -> (ParserError -> r) -> (ParserState -> a -> r) -> r)

instance Functor Parser where
  fmap f (Parser k) =
    Parser $ \st kerr ksucc ->
      k st kerr (\st' a -> ksucc st' (f a))

instance Applicative Parser where
  pure a = Parser $ \st _ k -> k st a
  Parser k1 <*> Parser k2 =
    Parser $ \st kerr ksucc ->
      k1 st kerr $ \st' f ->
        k2 st' kerr $ \st'' a ->
          ksucc st'' (f a)

instance Monad Parser where
  return = pure
  Parser k1 >>= k2 =
    Parser $ \st kerr ksucc ->
      k1 st kerr $ \st' a -> do
        let Parser k3 = k2 a
        k3 st' kerr ksucc

runParser :: ParserState -> Parser a -> Either ParserError a
runParser st (Parser k) = k st Left (const Right)

parseError :: (SourceToken, [String]) -> Parser a
parseError (tok, alts) = Parser $ \_ kerr _ ->
  kerr $ ParserError
    { errPos = srcStart . tokRange . fst $ tok
    , errTok = Just tok
    , errAlts = alts
    , errMsg = "Unexpected token"
    }

parseFail :: SourceToken -> String -> Parser a
parseFail tok msg = Parser $ \_ kerr _ ->
  kerr $ ParserError
    { errPos = srcStart $ tokRange $ fst tok
    , errTok = Just tok
    , errAlts = []
    , errMsg = msg
    }

getLeadingComments :: Parser [Comment LineFeed]
getLeadingComments = Parser $ \st _ ksucc -> ksucc st (parserLeading st)
