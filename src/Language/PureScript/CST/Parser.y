{
module Language.PureScript.CST.Parser
  ( parseType
  , parseKind
  , parseExpr
  , parseModule
  , parse
  ) where

import Prelude

import Data.Foldable (foldl', for_)
import Data.Text (Text)
import Language.PureScript.CST.Errors
import Language.PureScript.CST.Lexer
import Language.PureScript.CST.Monad
import Language.PureScript.CST.Positions
import Language.PureScript.CST.Types
import Language.PureScript.CST.Utils
}

%name parseKind kind
%name parseType type
%name parseExpr expr
%name parseModule module
%tokentype { SourceToken }
%monad { Parser }
%error { parseError }
%lexer { lexer } { SourceToken _ TokEof }

%token
  '('             { SourceToken _ TokLeftParen }
  ')'             { SourceToken _ TokRightParen }
  '{'             { SourceToken _ TokLeftBrace }
  '}'             { SourceToken _ TokRightBrace }
  '['             { SourceToken _ TokLeftSquare }
  ']'             { SourceToken _ TokRightSquare }
  '\{'            { SourceToken _ TokLayoutStart }
  '\}'            { SourceToken _ TokLayoutEnd }
  '\;'            { SourceToken _ TokLayoutSep }
  '<-'            { SourceToken _ (TokLeftArrow _) }
  '->'            { SourceToken _ (TokRightArrow _) }
  '<='            { SourceToken _ (TokOperator [] sym) | sym == "<=" || sym == "⇐" }
  '=>'            { SourceToken _ (TokRightFatArrow _) }
  ':'             { SourceToken _ (TokOperator [] ":") }
  '::'            { SourceToken _ (TokDoubleColon _) }
  '='             { SourceToken _ TokEquals }
  '|'             { SourceToken _ TokPipe }
  '`'             { SourceToken _ TokTick }
  '.'             { SourceToken _ TokDot }
  ','             { SourceToken _ TokComma }
  '_'             { SourceToken _ TokUnderscore }
  '\\'            { SourceToken _ TokBackslash }
  '-'             { SourceToken _ (TokOperator [] "-") }
  '@'             { SourceToken _ (TokOperator [] "@") }
  '#'             { SourceToken _ (TokOperator [] "#") }
  'ado'           { SourceToken _ (TokLowerName _ "ado") }
  'as'            { SourceToken _ (TokLowerName [] "as") }
  'case'          { SourceToken _ (TokLowerName [] "case") }
  'class'         { SourceToken _ (TokLowerName [] "class") }
  'data'          { SourceToken _ (TokLowerName [] "data") }
  'derive'        { SourceToken _ (TokLowerName [] "derive") }
  'do'            { SourceToken _ (TokLowerName _ "do") }
  'else'          { SourceToken _ (TokLowerName [] "else") }
  'false'         { SourceToken _ (TokLowerName [] "false") }
  'forall'        { SourceToken _ (TokForall ASCII) }
  'forallu'       { SourceToken _ (TokForall Unicode) }
  'foreign'       { SourceToken _ (TokLowerName [] "foreign") }
  'hiding'        { SourceToken _ (TokLowerName [] "hiding") }
  'import'        { SourceToken _ (TokLowerName [] "import") }
  'if'            { SourceToken _ (TokLowerName [] "if") }
  'in'            { SourceToken _ (TokLowerName [] "in") }
  'infix'         { SourceToken _ (TokLowerName [] "infix") }
  'infixl'        { SourceToken _ (TokLowerName [] "infixl") }
  'infixr'        { SourceToken _ (TokLowerName [] "infixr") }
  'instance'      { SourceToken _ (TokLowerName [] "instance") }
  'kind'          { SourceToken _ (TokLowerName [] "kind") }
  'let'           { SourceToken _ (TokLowerName [] "let") }
  'module'        { SourceToken _ (TokLowerName [] "module") }
  'newtype'       { SourceToken _ (TokLowerName [] "newtype") }
  'of'            { SourceToken _ (TokLowerName [] "of") }
  'then'          { SourceToken _ (TokLowerName [] "then") }
  'true'          { SourceToken _ (TokLowerName [] "true") }
  'type'          { SourceToken _ (TokLowerName [] "type") }
  'where'         { SourceToken _ (TokLowerName [] "where") }
  '(->)'          { SourceToken _ (TokSymbolArr _) }
  '(..)'          { SourceToken _ (TokSymbolName [] "..") }
  IDENT           { SourceToken _ (TokLowerName [] _) }
  QUAL_IDENT      { SourceToken _ (TokLowerName _ _) }
  PROPER          { SourceToken _ (TokUpperName [] _) }
  QUAL_PROPER     { SourceToken _ (TokUpperName _ _) }
  SYMBOL          { SourceToken _ (TokSymbolName [] _) }
  QUAL_SYMBOL     { SourceToken _ (TokSymbolName _ _) }
  OPERATOR        { SourceToken _ (TokOperator [] _) }
  QUAL_OPERATOR   { SourceToken _ (TokOperator _ _) }
  LIT_HOLE        { SourceToken _ (TokHole _) }
  LIT_CHAR        { SourceToken _ (TokChar _ _) }
  LIT_STRING      { SourceToken _ (TokString _ _) }
  LIT_RAW_STRING  { SourceToken _ (TokRawString _) }
  LIT_INT         { SourceToken _ (TokInt _ _) }
  LIT_NUMBER      { SourceToken _ (TokNumber _ _) }

%%

many(a) :: { [_] }
  : many0(a) { reverse $1 }

many0(a) :: { [_] }
  : a { [$1] }
  | many0(a) a { $2 : $1 }

manySep(a, sep) :: { [_] }
  : manySep0(a, sep) { reverse $1 }

manySep0(a, sep) :: { [_] }
  : a { [$1] }
  | manySep0(a, sep) sep a { $3 : $1 }

manySepOrEmpty(a, sep) :: { [_] }
  : {- empty -} { [] }
  | manySep(a, sep) { $1 }

manyOrEmpty(a) :: { [_] }
  : {- empty -} { [] }
  | many(a) { $1 }

sep(a, s) :: { Separated _ }
  : sep0(a, s) { separated $1 }

sep0(a, s) :: { [(SourceToken, _)] }
  : a { [(placeholder, $1)] }
  | sep0(a, s) s a { ($2, $3) : $1 }

delim(a, b, c, d) :: { Delimited _ }
  : a d { Wrapped $1 Nothing $2 }
  | a sep(b, c) d { Wrapped $1 (Just $2) $3 }

properIdent :: { Ident }
  : PROPER { toIdent $1 }
  | QUAL_PROPER { toIdent $1 }

proper :: { Ident }
  : PROPER { toIdent $1 }

ident :: { Ident }
  : IDENT { toIdent $1 }
  | QUAL_IDENT { toIdent $1 }
  | 'as' { toIdent $1 }
  | 'hiding' { toIdent $1 }
  | 'kind' { toIdent $1 }

var :: { Ident }
  : IDENT {% toVar $1 }
  | 'as' {% toVar $1 }
  | 'kind' {% toVar $1 }
  | 'hiding' { toIdent $1 }

op :: { Ident }
  : OPERATOR { toOperator $1 }
  | '<=' { toOperator $1 }
  | '-' { toOperator $1 }
  | '@' { toOperator $1 }
  | '#' { toOperator $1 }
  | ':' { toOperator $1 }

opIdent :: { Ident }
  : op { $1 }
  | QUAL_OPERATOR { toOperator $1 }

symbol :: { Ident }
  : SYMBOL { toIdent $1 }
  | '(..)' { toIdent $1 }

symbolIdent :: { Ident }
  : symbol { $1 }
  | QUAL_SYMBOL { toIdent $1 }

label :: { Ident }
  : IDENT { toLabel $1 }
  | LIT_STRING { toLabel $1 }
  | LIT_RAW_STRING { toLabel $1 }
  | 'ado' { toLabel $1 }
  | 'as' { toLabel $1 }
  | 'case' { toLabel $1 }
  | 'class' { toLabel $1 }
  | 'data' { toLabel $1 }
  | 'derive' { toLabel $1 }
  | 'do' { toLabel $1 }
  | 'else' { toLabel $1 }
  | 'false' { toLabel $1 }
  | 'forall' { toLabel $1 }
  | 'foreign' { toLabel $1 }
  | 'hiding' { toLabel $1 }
  | 'import' { toLabel $1 }
  | 'if' { toLabel $1 }
  | 'in' { toLabel $1 }
  | 'infix' { toLabel $1 }
  | 'infixl' { toLabel $1 }
  | 'infixr' { toLabel $1 }
  | 'instance' { toLabel $1 }
  | 'kind' { toLabel $1 }
  | 'let' { toLabel $1 }
  | 'module' { toLabel $1 }
  | 'newtype' { toLabel $1 }
  | 'of' { toLabel $1 }
  | 'then' { toLabel $1 }
  | 'true' { toLabel $1 }
  | 'type' { toLabel $1 }
  | 'where' { toLabel $1 }

hole :: { Ident }
  : LIT_HOLE { toIdent $1 }

string :: { (SourceToken, Text) }
  : LIT_STRING { toString $1 }
  | LIT_RAW_STRING { toString $1 }

char :: { (SourceToken, Char) }
  : LIT_CHAR { toChar $1 }

number :: { (SourceToken, Either Integer Double) }
  : LIT_INT { toNumber $1 }
  | LIT_NUMBER { toNumber $1 }

int :: { (SourceToken, Integer) }
  : LIT_INT { toInt $1 }

boolean :: { (SourceToken, Bool) }
  : 'true' { toBoolean $1 }
  | 'false' { toBoolean $1 }

kind :: { Kind () }
  : kind0 { $1 }
  | kind0 '->' kind { KindArr () $1 $2 $3 }

kind0 :: { Kind () }
  : properIdent { KindName () $1 }
  | '#' kind0 { KindRow () $1 $2 }
  | '(' kind ')' { KindParens () (Wrapped $1 $2 $3) }

type :: { Type () }
  : type0 { $1 }
  | forall many(typeVarBinding) '.' type { TypeForall () $1 $2 $3 $4 }

type0 :: { Type () }
  : type1 { $1 }
  | type1 '->' type { TypeArr () $1 $2 $3 }
  | type1 '=>' type { TypeConstrained () $1 $2 $3 }

type1 :: { Type () }
  : type2 { $1 }
  | type1 opIdent type2 { TypeOp () $1 $2 $3 }

type2 :: { Type () }
  : typeAtom { $1 }
  | type2 typeAtom { TypeApp () $1 $2 }

typeAtom :: { Type ()}
  : '_' { TypeWildcard () $1 }
  | var { TypeVar () $1 }
  | properIdent { TypeConstructor () $1 }
  | symbolIdent { TypeOpName () $1 }
  | string { uncurry (TypeString ()) $1 }
  | hole { TypeHole () $1 }
  | '(->)' { TypeArrName () $1 }
  | '{' row '}' { TypeRecord () (Wrapped $1 $2 $3) }
  | '(' row ')' { TypeRow () (Wrapped $1 $2 $3) }
  | '(' type ')' { TypeParens () (Wrapped $1 $2 $3) }
  | '(' typeKindedAtom '::' kind ')' { TypeParens () (Wrapped $1 (TypeKinded () $2 $3 $4) $5) }

typeKindedAtom :: { Type () }
  : '_' { TypeWildcard () $1 }
  | properIdent { TypeConstructor () $1 }
  | symbolIdent { TypeOpName () $1 }
  | hole { TypeHole () $1 }
  | '{' row '}' { TypeRecord () (Wrapped $1 $2 $3) }
  | '(' row ')' { TypeRow () (Wrapped $1 $2 $3) }
  | '(' type ')' { TypeParens () (Wrapped $1 $2 $3) }
  | '(' typeKindedAtom '::' kind ')' { TypeParens () (Wrapped $1 (TypeKinded () $2 $3 $4) $5) }

row :: { Row () }
  : {- empty -} { Row Nothing Nothing }
  | '|' type { Row Nothing (Just ($1, $2)) }
  | sep(rowLabel, ',') { Row (Just $1) Nothing }
  | sep(rowLabel, ',') '|' type { Row (Just $1) (Just ($2, $3)) }

rowLabel :: { Labeled (Type ()) }
  : label '::' type { Labeled $1 $2 $3 }

typeVarBinding :: { TypeVarBinding () }
  : var { TypeVarName $1 }
  | '(' var '::' kind ')' { TypeVarKinded (Wrapped $1 (Labeled $2 $3 $4) $5) }

forall :: { SourceToken }
  : 'forall' { $1 }
  | 'forallu' { $1 }

exprWhere :: { Expr () }
  : expr { $1 }
  | expr 'where' '\{' manySep(letBinding, '\;') '\}' { ExprWhere () (Where $1 $2 $4) }

expr :: { Expr () }
  : expr0 { $1 }
  | expr0 '::' type { ExprTyped () $1 $2 $3 }

expr0 :: { Expr () }
  : expr1 { $1 }
  | expr0 opIdent expr1 { ExprOp () $1 $2 $3 }

expr1 :: { Expr () }
  : expr2 { $1 }
  | expr1 '`' exprBacktick '`' expr2 { ExprInfix () $1 (Wrapped $2 $3 $4) $5 }

exprBacktick :: { Expr () }
  : expr2 { $1 }
  | exprBacktick opIdent expr2 { ExprOp () $1 $2 $3 }

expr2 :: { Expr () }
  : expr3 { $1 }
  | '-' expr3 { ExprNegate () $1 $2 }

expr3 :: { Expr () }
  : expr4 { $1 }
  | expr3 expr4
      { -- Record application/updates can introduce a function application
        -- associated to the right, so we need to correct it.
        case $2 of
          ExprApp _ lhs rhs ->
            ExprApp () (ExprApp () $1 lhs) rhs
          _ -> ExprApp () $1 $2
      }

expr4 :: { Expr () }
  : expr5 { $1 }
  | 'if' expr 'then' expr 'else' expr { ExprIf () (IfThenElse $1 $2 $3 $4 $5 $6) }
  | 'do' '\{' manySep(doStatement, '\;') '\}' { ExprDo () (DoBlock $1 $3) }
  | 'ado' '\{' manySepOrEmpty(doStatement, '\;') '\}' 'in' expr { ExprAdo () (AdoBlock $1 $3 $5 $6) }
  | '\\' expr0 '->' expr {% do bs <- toBinderAtoms $2; pure $ ExprLambda () (Lambda $1 bs $3 $4) }
  | 'let' '\{' manySep(letBinding, '\;') '\}' 'in' expr { ExprLet () (LetIn $1 $3 $5 $6) }
  | 'case' sep(expr, ',') 'of' '\{' manySep(caseBranch, '\;') '\}' { ExprCase () (CaseOf $1 $2 $3 $5) }
  -- These special cases handle some idiosynchratic syntax that the current
  -- parse allows. Technically the parser allows the rhs of a case branch to
  -- be at any level, but this is ambiguous. We allow it in the case of a
  -- singleton case, since this is used in the wild.
  | 'case' sep(expr, ',') 'of' '\{' sep(expr0, ',') '->' '\}' exprWhere
      {% do bs <- traverse toBinder $5; pure $ ExprCase () (CaseOf $1 $2 $3 [(bs, Unconditional $6 $8)]) }
  | 'case' sep(expr, ',') 'of' '\{' sep(expr0, ',') '\}' guarded('->')
      {% do bs <- traverse toBinder $5; pure $ ExprCase () (CaseOf $1 $2 $3 [(bs, $7)]) }

expr5 :: { Expr () }
  : expr6 { $1 }
  | expr6 '{' '}' { ExprApp () $1 (ExprRecord () (Wrapped $2 Nothing $3)) }
  | expr6 '{' sep(recordUpdateOrLabel, ',') '}'
      {% toRecordFields $3 >>= \case
          Left xs -> pure $ ExprApp () $1 (ExprRecord () (Wrapped $2 (Just xs) $4))
          Right xs -> pure $ ExprRecordUpdate () $1 (Wrapped $2 xs $4)
      }

expr6 :: { Expr () }
  : exprAtom { $1 }
  | exprAtom '.' sep(label, '.') { ExprRecordAccessor () (RecordAccessor $1 $2 $3) }

exprAtom :: { Expr () }
  : '_' { ExprSection () $1 }
  | hole { ExprHole () $1 }
  | ident { ExprIdent () $1 }
  | properIdent { ExprConstructor () $1 }
  | symbolIdent { ExprOpName () $1 }
  | boolean { uncurry (ExprBoolean ()) $1 }
  | char { uncurry (ExprChar ()) $1 }
  | string { uncurry (ExprString ()) $1 }
  | number { uncurry (ExprNumber ()) $1 }
  | array { ExprArray () $1 }
  | record { ExprRecord () $1 }
  | '(' expr ')' { ExprParens () (Wrapped $1 $2 $3) }
  | error {%% recover ErrExpr unexpectedExpr }

array :: { Delimited (Expr ()) }
  : delim('[', expr, ',', ']') { $1 }

record :: { Delimited (RecordLabeled (Expr ())) }
  : delim('{', recordLabel, ',', '}') { $1 }

recordLabel :: { RecordLabeled (Expr ()) }
  : label {% fmap RecordPun (labelToVar $1) }
  | label '=' expr {% do addFailure [$2] ErrRecordUpdateInCtr; pure $ RecordPun $ unexpected $ identTok $1 }
  | label ':' expr { RecordField $1 $2 $3 }

recordUpdateOrLabel :: { Either (RecordLabeled (Expr ())) (RecordUpdate ()) }
  : label ':' expr { Left (RecordField $1 $2 $3) }
  | label {% fmap (Left . RecordPun) (labelToVar $1) }
  | label '=' expr { Right (RecordUpdateLeaf $1 $2 $3) }
  | label '{' sep(recordUpdate, ',') '}' { Right (RecordUpdateBranch $1 (Wrapped $2 $3 $4)) }

recordUpdate :: { RecordUpdate () }
  : label '=' expr { RecordUpdateLeaf $1 $2 $3 }
  | label '{' sep(recordUpdate, ',') '}' { RecordUpdateBranch $1 (Wrapped $2 $3 $4) }

letBinding :: { LetBinding () }
  : var '::' type { LetBindingSignature () (Labeled $1 $2 $3) }
  | expr0 guarded('=') {% toLetBinding $1 $2 }
  | expr0 {%% recover ErrLetBinding (unexpectedLetBinding . (fst (exprRange $1) :)) }

caseBranch :: { (Separated (Binder ()), Guarded ()) }
  : sep(expr0, ',') guarded('->') {% do bs <- traverse toBinder $1; pure (bs, $2) }

guarded(a) :: { Guarded () }
  : a exprWhere { Unconditional $1 $2 }
  | many(guardedExpr(a)) { Guarded $1 }

guardedExpr(a) :: { GuardedExpr () }
  : '|' sep(patternGuard, ',') a exprWhere { GuardedExpr $1 $2 $3 $4 }

patternGuard :: { PatternGuard () }
  : expr0 { PatternGuard Nothing $1 }
  | expr '<-' expr0 {% do b <- toBinder $1; pure $ PatternGuard (Just (b, $2)) $3 }

doStatement :: { DoStatement () }
  : 'let' '\{' manySep(letBinding, '\;') '\}' { DoLet $1 $3 }
  | expr { DoDiscard $1 }
  | expr '<-' expr {% do b <- toBinder $1; pure $ DoBind b $2 $3 }

module :: { Module () }
  : 'module' properIdent exports 'where' '\{' moduleDecls '\}'
      {% fmap (uncurry (Module () $1 $2 $3 $4) $6) getLeadingComments }

moduleDecls :: { ([ImportDecl ()], [Declaration ()]) }
  : manySep(moduleDecl, '\;') {% toModuleDecls $1 }
  | {- empty -} { ([], []) }

moduleDecl :: { TmpModuleDecl a }
  : importDecl { TmpImport $1 }
  | decl { TmpDecl $1 }
  | 'else' '\;' decl { TmpChain $1 $3 }
  | 'else' decl { TmpChain $1 $2 }

exports :: { Maybe (DelimitedNonEmpty (Export ())) }
  : {- empty -} { Nothing }
  | '(' sep(export, ',') ')' { Just (Wrapped $1 $2 $3) }

export :: { Export () }
  : var { ExportValue () $1 }
  | symbol { ExportOp () $1 }
  | proper { ExportType () $1 Nothing }
  | proper dataMembers { ExportType () $1 (Just $2) }
  | 'type' symbol { ExportTypeOp () $1 $2 }
  | 'class' proper { ExportClass () $1 $2 }
  | 'kind' proper { ExportKind () $1 $2 }
  | 'module' properIdent { ExportModule () $1 $2 }

dataMembers :: { (DataMembers ()) }
 : '(..)' { DataAll () $1 }
 | '(' ')' { DataEnumerated () (Wrapped $1 Nothing $2) }
 | '(' sep(proper, ',') ')' { DataEnumerated () (Wrapped $1 (Just $2) $3) }

importDecl :: { ImportDecl () }
  : 'import' properIdent imports { ImportDecl () $1 $2 $3 Nothing }
  | 'import' properIdent imports 'as' properIdent { ImportDecl () $1 $2 $3 (Just ($4, $5)) }

imports :: { Maybe (Maybe SourceToken, DelimitedNonEmpty (Import ())) }
  : {- empty -} { Nothing }
  | '(' sep(import, ',') ')' { Just (Nothing, Wrapped $1 $2 $3) }
  | 'hiding' '(' sep(import, ',') ')' { Just (Just $1, Wrapped $2 $3 $4) }

import :: { Import () }
  : var { ImportValue () $1 }
  | symbol { ImportOp () $1 }
  | proper { ImportType () $1 Nothing }
  | proper dataMembers { ImportType () $1 (Just $2) }
  | 'type' symbol { ImportTypeOp () $1 $2 }
  | 'class' proper { ImportClass () $1 $2 }
  | 'kind' proper { ImportKind () $1 $2 }

decl :: { Declaration () }
  : dataHead { DeclData () $1 Nothing }
  | dataHead '=' sep(dataCtor, '|') { DeclData () $1 (Just ($2, $3)) }
  | typeHead '=' type {% do checkNoWildcards $3; pure (DeclType () $1 $2 $3) }
  | newtypeHead '=' properIdent typeAtom {% do checkNoWildcards $4; pure (DeclNewtype () $1 $2 $3 $4) }
  | classHead {% do checkFundeps $1; pure $ DeclClass () $1 Nothing }
  | classHead 'where' '\{' manySep(classMember, '\;') '\}'
      {% do checkFundeps $1; pure $ DeclClass () $1 (Just ($2, $4)) }
  | instHead { DeclInstanceChain () (Separated (Instance $1 Nothing) []) }
  | instHead 'where' '\{' manySep(instBinding, '\;') '\}'
      { DeclInstanceChain () (Separated (Instance $1 (Just ($2, $4))) []) }
  | 'derive' instHead { DeclDerive () $1 Nothing $2 }
  | 'derive' 'newtype' instHead { DeclDerive () $1 (Just $2) $3 }
  | ident '::' type { DeclSignature () (Labeled $1 $2 $3) }
  | expr0 guarded('=') {% toDecl DeclValue unexpectedDecl $1 $2 }
  | fixity { DeclFixity () $1 }
  | 'foreign' 'import' foreign { DeclForeign () $1 $2 $3 }
  | expr0 error {%% recover ErrExprInDecl (unexpectedDecl . (fst (exprRange $1) :)) }
  | error {%% recover ErrDecl unexpectedDecl }

dataHead :: { DataHead () }
  : 'data' properIdent manyOrEmpty(typeVarBinding) { DataHead $1 $2 $3 }

typeHead :: { DataHead () }
  : 'type' properIdent manyOrEmpty(typeVarBinding) { DataHead $1 $2 $3 }

newtypeHead :: { DataHead () }
  : 'newtype' properIdent manyOrEmpty(typeVarBinding) { DataHead $1 $2 $3 }

dataCtor :: { DataCtor () }
  : properIdent manyOrEmpty(typeAtom)
      {% do for_ $2 checkNoWildcards; pure (DataCtor () $1 $2) }

classHead :: { ClassHead () }
  : 'class' classNameAndVars fundeps { ClassHead $1 Nothing (fst $2) (snd $2) $3 }
  -- We need to inline constraints due to the reduce/reduce conflict between
  -- the class name and vars and constraint syntax.
  | 'class' classNameAndVars '<=' classNameAndVars fundeps
      { ClassHead $1
          (Just (One (foldl' (TypeApp ()) (TypeConstructor () (fst $2)) (fmap varToType (snd $2))), $3))
          (fst $4)
          (snd $4)
          $5 }
  | 'class' '(' sep(constraint, ',') ')' '<=' classNameAndVars fundeps
      { ClassHead $1 (Just (Many (Wrapped $2 $3 $4), $5)) (fst $6) (snd $6) $7 }

classNameAndVars :: { (Ident, [TypeVarBinding ()]) }
  : properIdent manyOrEmpty(typeVarBinding) { ($1, $2) }

fundeps :: { Maybe (SourceToken, Separated ClassFundep) }
  : {- empty -} { Nothing }
  | '|' sep(fundep, ',') { Just ($1, $2) }

fundep :: { ClassFundep }
  : '->' many(ident) { ClassFundep [] $1 $2 }
  | many(ident) '->' many(ident) { ClassFundep $1 $2 $3 }

classMember :: { Labeled (Type ()) }
  : ident '::' type {% do checkNoWildcards $3; pure (Labeled $1 $2 $3) }

instHead :: { InstanceHead () }
  : 'instance' ident '::' constraints '=>' properIdent manyOrEmpty(typeAtom)
      { InstanceHead $1 $2 $3 (Just ($4, $5)) $6 $7 }
  | 'instance' ident '::' properIdent manyOrEmpty(typeAtom)
      { InstanceHead $1 $2 $3 Nothing $4 $5 }

constraints :: { OneOrDelimited (Type ()) }
  : constraint { One $1 }
  | '(' sep(constraint, ',') ')' { Many (Wrapped $1 $2 $3) }

constraint :: { Type () }
  : properIdent { TypeConstructor () $1 }
  | properIdent many(typeAtom)
      {% do for_ $2 checkNoWildcards; pure (foldl' (TypeApp ()) (TypeConstructor () $1) $2) }

instBinding :: { InstanceBinding () }
  : ident '::' type { InstanceBindingSignature () (Labeled $1 $2 $3) }
  | expr0 guarded('=') {% toDecl InstanceBindingName unexpectedInstBinding $1 $2 }

fixity :: { FixityFields () }
  : infix int var 'as' op { FixityFields $1 $2 Nothing $3 $4 $5 }
  | infix int proper 'as' op { FixityFields $1 $2 Nothing $3 $4 $5 }
  | infix int 'type' proper 'as' op { FixityFields $1 $2 (Just $3) $4 $5 $6 }

infix :: { SourceToken }
  : 'infix' { $1 }
  | 'infixl' { $1 }
  | 'infixr' { $1 }

foreign :: { Foreign () }
  : ident '::' type { ForeignValue (Labeled $1 $2 $3) }
  | 'data' properIdent '::' kind { ForeignData $1 (Labeled $2 $3 $4) }
  | 'kind' properIdent { ForeignKind $1 $2 }

{
lexer :: (SourceToken -> Parser a) -> Parser a
lexer k = munch >>= k

parse :: Text -> Either [ParserError] (Module ())
parse src = runParser (initialParserState src) parseModule
}
