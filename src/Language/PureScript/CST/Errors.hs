module Language.PureScript.CST.Errors
  ( ParserError(..)
  , ParserErrorType(..)
  , prettyPrintError
  ) where

import Prelude

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Char (isSpace)
import Language.PureScript.CST.Layout
import Language.PureScript.CST.Print
import Language.PureScript.CST.Types

data ParserErrorType
  = ErrExpr
  | ErrDecl
  | ErrWildcardInType
  | ErrHoleInType
  | ErrExprInLabel
  | ErrExprInBinder
  | ErrExprInDeclOrBinder
  | ErrExprInDecl
  | ErrBinderInDecl
  | ErrRecordUpdateInCtr
  | ErrRecordPunInUpdate
  | ErrRecordCtrInUpdate
  | ErrElseInDecl
  | ErrInstanceNameMismatch
  | ErrUnknownFundep
  | ErrImportInDecl
  | ErrGuardInLetBinder
  | ErrKeywordVar
  | ErrKeywordSymbol
  | ErrLetBinding
  | ErrToken
  | ErrLineFeedInString
  | ErrAstralCodePointInChar
  | ErrCharEscape
  | ErrNumberOutOfRange
  | ErrLeadingZero
  | ErrExpectedExponent
  | ErrReservedSymbol Text
  | ErrCharInGap Char
  | ErrLexeme (Maybe String) [String]
  | ErrEof
  deriving (Show, Eq, Ord)

data ParserError = ParserError
  { errRange :: SourceRange
  , errToks :: [SourceToken]
  , errStack :: LayoutStack
  , errType :: ParserErrorType
  } deriving (Show, Eq)

prettyPrintError :: ParserError -> String
prettyPrintError pe@(ParserError { errRange }) =
  prettyPrintErrorMessage pe <> " at " <> errPos
  where
  errPos = case errRange of
    SourceRange (SourcePos line col) _ ->
      "line " <> show line <> ", column " <> show col

prettyPrintErrorMessage :: ParserError -> String
prettyPrintErrorMessage (ParserError {..}) = case errType of
  ErrWildcardInType ->
    "Unexpected wildcard in type; type wildcards are only allowed in value annotations"
  ErrHoleInType ->
    "Unexpected hole in type; type holes are only allowed in value annotations"
  ErrExprInLabel ->
    "Expected labeled expression or pun, saw expression"
  ErrExprInBinder ->
    "Expected pattern, saw expression"
  ErrExprInDeclOrBinder ->
    "Expected declaration or pattern, saw expression"
  ErrExprInDecl ->
    "Expected declaration, saw expression"
  ErrBinderInDecl ->
    "Expected declaration, saw pattern"
  ErrRecordUpdateInCtr ->
    "Expected ':', saw '='"
  ErrRecordPunInUpdate ->
    "Expected record update, saw pun"
  ErrRecordCtrInUpdate ->
    "Expected '=', saw ':'"
  ErrElseInDecl ->
    "Expected declaration, saw 'else'"
  ErrInstanceNameMismatch ->
    "All instances in a chain must implement the same type class"
  ErrUnknownFundep ->
    "Unknown type variable in functional dependency"
  ErrImportInDecl ->
    "Expected declaration, saw 'import'"
  ErrGuardInLetBinder ->
    "Unexpected guard in let pattern"
  ErrKeywordVar ->
    "Expected variable, saw keyword"
  ErrKeywordSymbol ->
    "Expected symbol, saw reserved symbol"
  ErrEof ->
    "Unexpected end of input"
  ErrLexeme (Just (hd : _)) _ | isSpace hd ->
    "Illegal whitespace character " <> show hd
  ErrLexeme (Just a) _ ->
    "Unexpected " <> a
  ErrLineFeedInString ->
    "Unexpected line feed in string literal"
  ErrAstralCodePointInChar ->
    "Illegal astral code point in character literal"
  ErrCharEscape ->
    "Illegal character escape code"
  ErrNumberOutOfRange ->
    "Number literal is out of range"
  ErrLeadingZero ->
    "Unexpected leading zeros"
  ErrExpectedExponent ->
    "Expected exponent"
  ErrReservedSymbol sym ->
    "Unexpected reserved symbol '" <> Text.unpack sym <> "'"
  ErrCharInGap ch ->
    "Unexpected character '" <> [ch] <> "' in gap"
  ErrLexeme _ _ ->
    basicError
  ErrLetBinding ->
    basicError
  ErrToken
    | SourceToken _ (TokLeftArrow _) : _ <- errToks ->
        "Unexpected \"<-\" in expression, perhaps due to a missing 'do' or 'ado' keyword"
  ErrToken ->
    basicError
  ErrExpr
    | SourceToken _ TokLayoutEnd : _ <- errToks ->
        case errStack of
          [] ->
            "Expected expression, saw end of module"
          _ ->
            "Expected expression, saw dedent"
  ErrExpr
    | SourceToken _ TokLayoutSep : _ <- errToks ->
        case errStack of
          (_, LytDo) : _ ->
            "Expected expression, saw new do statement"
          (_, LytAdo) : _ ->
            "Expected expression, saw new ado statement"
          (_, LytCaseBinders) : _ ->
            "Expected expression, saw new case branch"
          (_, LytLet) : _ ->
            "Expected expression, saw new declaration"
          (_, LytWhere) : _ ->
            "Expected expression, saw new declaration"
          _ ->
            basicError
  ErrExpr ->
    basicError
  ErrDecl ->
    basicError

  where
  basicError = case errToks of
    tok : _ -> basicTokError (tokValue tok)
    [] -> "Unexpected input"

  basicTokError = \case
    TokLayoutStart -> "Unexpected or mismatched indentation"
    TokLayoutSep   -> "Unexpected or mismatched indentation"
    TokLayoutEnd   -> "Unexpected or mismatched indentation"
    TokEof         -> "Unexpected end of input"
    tok            -> "Unexpected token '" <> Text.unpack (printToken tok) <> "'"
