{
module Language.PureScript.CST.Parser
  ( parseType
  , parseKind
  , parseExpr
  ) where

import Prelude

import Data.Text (Text)
import Language.PureScript.CST.Types
import Language.PureScript.CST.Utils
}

%name parseKind Kind
%name parseType Type
%name parseExpr Expr
%tokentype { SourceToken }
%errorhandlertype explist
%error { (error . show) }

%token
  '('             { (_, TokLeftParen) }
  ')'             { (_, TokRightParen) }
  '{'             { (_, TokLeftBrace) }
  '}'             { (_, TokRightBrace) }
  '['             { (_, TokLeftSquare) }
  ']'             { (_, TokRightSquare) }
  '<-'            { (_, TokLeftArrow _) }
  '->'            { (_, TokRightArrow _) }
  '<='            { (_, TokSymbol [] sym) | sym == "<=" || sym == "⇐" }
  '=>'            { (_, TokRightFatArrow _) }
  ':'             { (_, TokSymbol [] ":") }
  '::'            { (_, TokDoubleColon _) }
  '='             { (_, TokEquals) }
  '|'             { (_, TokPipe) }
  '`'             { (_, TokTick) }
  '.'             { (_, TokDot) }
  ','             { (_, TokComma) }
  '_'             { (_, TokUnderscore) }
  '#'             { (_, TokSymbol [] "#") }
  ado             { (_, TokLowerName _ "ado") }
  case            { (_, TokLowerName [] "case") }
  class           { (_, TokLowerName [] "class") }
  data            { (_, TokLowerName [] "data") }
  derive          { (_, TokLowerName [] "derive") }
  do              { (_, TokLowerName _ "do") }
  else            { (_, TokLowerName [] "else") }
  false           { (_, TokLowerName [] "false") }
  forall          { (_, TokLowerName [] "forall") }
  forallu         { (_, TokSymbol [] "∀") }
  foreign         { (_, TokLowerName [] "foreign") }
  import          { (_, TokLowerName [] "import") }
  if              { (_, TokLowerName [] "if") }
  in              { (_, TokLowerName [] "in") }
  infix           { (_, TokLowerName [] "infix") }
  infixl          { (_, TokLowerName [] "infixl") }
  infixr          { (_, TokLowerName [] "infixr") }
  instance        { (_, TokLowerName [] "instance") }
  let             { (_, TokLowerName [] "forall") }
  module          { (_, TokLowerName [] "module") }
  newtype         { (_, TokLowerName [] "newtype") }
  of              { (_, TokLowerName [] "of") }
  then            { (_, TokLowerName [] "then") }
  true            { (_, TokLowerName [] "true") }
  type            { (_, TokLowerName [] "type") }
  where           { (_, TokLowerName [] "where") }
  LOWER_NAME      { (_, TokLowerName _ _) }
  UPPER_NAME      { (_, TokUpperName _ _) }
  SYMBOL          { (_, TokSymbol _ _) }
  LIT_HOLE        { (_, TokHole _) }
  LIT_CHAR        { (_, TokChar _ _) }
  LIT_STRING      { (_, TokString _ _) }
  LIT_RAW_STRING  { (_, TokRawString _) }
  LIT_INT         { (_, TokInt _ _) }
  LIT_NUMBER      { (_, TokNumber _ _) }

%%

Many(a)                              :: { [_] }
  : Many0(a)                            { reverse $1 }

Many0(a)                             :: { [_] }
  : a                                   { [$1] }
  | Many0(a) a                          { $2 : $1 }

Separated(a, sep)                    :: { Separated _ }
  : Separated0(a, sep)                  { separated $1 }

Separated0(a, sep)                   :: { [(SourceToken, _)] }
  : a                                   { [(placeholder, $1)] }
  | Separated0(a, sep) sep a            { ($2, $3) : $1 }

Proper                               :: { Ident }
  : UPPER_NAME                          { toIdent $1 }

Var                                  :: { Ident }
  : LOWER_NAME                          { toVar $1 }

Symbol                               :: { Ident }
  : SYMBOL                              { toSymbol $1 }

Label                                :: { Ident }
  : LOWER_NAME                          { toLabel $1 }
  | LIT_STRING                          { toLabel $1 }
  | LIT_RAW_STRING                      { toLabel $1 }

Hole                                 :: { Ident }
  : LIT_HOLE                            { toIdent $1 }

String                               :: { (SourceToken, Text) }
  : LIT_STRING                          { toString $1 }
  | LIT_RAW_STRING                      { toString $1 }

Char                                 :: { (SourceToken, Char) }
  : LIT_CHAR                            { toChar $1 }

Number                               :: { (SourceToken, Either Integer Double) }
  : LIT_INT                             { toNumber $1 }
  | LIT_NUMBER                          { toNumber $1 }

Kind                                 :: { Kind () }
  : Kind0                               { $1 }
  | Kind0 '->' Kind                     { KindArr () $1 $2 $3 }

Kind0                                :: { Kind () }
  : Proper                              { KindName () $1 }
  | '#' Kind0                           { KindRow () $1 $2 }
  | '(' Kind ')'                        { KindParens () (Wrapped $1 $2 $3) }

Type                                 :: { Type () }
  : Type0                               { $1 }
  | Forall TypeVarBindings '.' Type     { TypeForall () $1 $2 $3 $4 }

Type0                                :: { Type () }
  : Type1                               { $1 }
  | Type1 '->' Type                     { TypeArr () $1 $2 $3 }
  | Type1 '=>' Type                     { TypeConstrained () $1 $2 $3 }

Type1                                :: { Type () }
  : Type2                               { $1 }
  | TypeAtom Symbol Type1               { TypeOp () $1 $2 $3 }

Type2                                :: { Type () }
  : TypeAtom                            { $1 }
  | Type2 TypeAtom                      { TypeApp () $1 $2 }

TypeAtom                             :: { Type ()}
  : '_'                                 { TypeWildcard () $1 }
  | Hole                                { TypeHole () $1 }
  | Var                                 { TypeVar () $1 }
  | Proper                              { TypeConstructor () $1 }
  | String                              { uncurry (TypeString ()) $1}
  | '{' Row '}'                         { TypeRecord () (Wrapped $1 $2 $3) }
  | '(' Row ')'                         { TypeRow () (Wrapped $1 $2 $3) }
  | '(' Type ')'                        { TypeParens () (Wrapped $1 $2 $3) }
  | '(' TypeKindedAtom '::' Kind ')'    { TypeParens () (Wrapped $1 (TypeKinded () $2 $3 $4) $5) }

TypeKindedAtom                       :: { Type () }
  : '_'                                 { TypeWildcard () $1 }
  | Hole                                { TypeHole () $1 }
  | Proper                              { TypeConstructor () $1 }
  | '{' Row '}'                         { TypeRecord () (Wrapped $1 $2 $3) }
  | '(' Row ')'                         { TypeRow () (Wrapped $1 $2 $3) }
  | '(' Type ')'                        { TypeParens () (Wrapped $1 $2 $3) }
  | '(' TypeKindedAtom '::' Kind ')'    { TypeParens () (Wrapped $1 (TypeKinded () $2 $3 $4) $5) }

Row                                  :: { Row () }
  : {- empty -}                         { Row Nothing Nothing }
  | '|' Type                            { Row Nothing (Just ($1, $2)) }
  | Separated(RowLabel, ',')            { Row (Just $1) Nothing }
  | Separated(RowLabel, ',') '|' Type   { Row (Just $1) (Just ($2, $3)) }

RowLabel                             :: { Labeled (Type ()) }
  : Label '::' Type                     { Labeled $1 $2 $3 }

TypeVarBindings                      :: { [TypeVarBinding ()] }
  : Many(TypeVarBinding)                { $1 }

TypeVarBinding                       :: { TypeVarBinding () }
  : Var                                 { TypeVarName $1 }
  | '(' Var '::' Kind ')'               { TypeVarKinded (Wrapped $1 (Labeled $2 $3 $4) $5) }

Forall                               :: { SourceToken }
  : forall                              { $1 }
  | forallu                             { $1 }

Expr                                 :: { Expr () }
  : Expr0                               { $1 }
  | Expr0 '::' Type                     { ExprTyped () $1 $2 $3 }

Expr0                                :: { Expr () }
  : if Expr1 then Expr1 else Expr0      { ExprIf () (IfThenElse $1 $2 $3 $4 $5 $6) }

Expr1
  : ExprAtom { $1 }

ExprAtom                             :: { Expr () }
  : Hole                                { ExprHole () $1 }
  | Var                                 { ExprVar () $1 }
  | Proper                              { ExprConstructor () $1 }
  | Char                                { uncurry (ExprChar ()) $1 }
  | String                              { uncurry (ExprString ()) $1 }
  | Number                              { uncurry (ExprNumber ()) $1 }
  | Array                               { ExprArray () $1 }
  | Record                              { ExprRecord () $1 }
  | '(' Expr ')'                        { ExprParens () (Wrapped $1 $2 $3) }

Array                                :: { Delimited (Expr ()) }
  : '[' ']'                             { Wrapped $1 Nothing $2 }
  | '[' Separated(Expr, ',') ']'        { Wrapped $1 (Just $2) $3 }

Record                               :: { Delimited (RecordLabeled (Expr ())) }
  : '{' '}'                             { Wrapped $1 Nothing $2 }
  | '{' Separated(Record0, ',') '}'     { Wrapped $1 (Just $2) $3 }

Record0                              :: { RecordLabeled (Expr ()) }
  : Label                               { RecordPun $1 }
  | Label ':' Expr                      { RecordField $1 $2 $3 }

-- RecordUpdate
