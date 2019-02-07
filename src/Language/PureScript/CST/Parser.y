{
module Language.PureScript.CST.Parser
  ( parseType
  , parseKind
  , parseExpr
  , parseModule
  , parse
  ) where

import Prelude

import Data.Foldable (foldl')
import Data.Text (Text)
import Language.PureScript.CST.Lexer
import Language.PureScript.CST.Monad
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
%lexer { lexer } { (_, TokEof) }

%token
  '('             { (_, TokLeftParen) }
  ')'             { (_, TokRightParen) }
  '{'             { (_, TokLeftBrace) }
  '}'             { (_, TokRightBrace) }
  '['             { (_, TokLeftSquare) }
  ']'             { (_, TokRightSquare) }
  '\{'            { (_, TokLayoutStart) }
  '\}'            { (_, TokLayoutEnd) }
  '\;'            { (_, TokLayoutSep) }
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
  '-'             { (_, TokSymbol [] "-") }
  '@'             { (_, TokSymbol [] "@") }
  '#'             { (_, TokSymbol [] "#") }
  '..'            { (_, TokSymbol [] "..") }
  'lambda'        { (_, TokSymbol [] "\\") }
  'ado'           { (_, TokLowerName _ "ado") }
  'as'            { (_, TokLowerName [] "as") }
  'case'          { (_, TokLowerName [] "case") }
  'class'         { (_, TokLowerName [] "class") }
  'data'          { (_, TokLowerName [] "data") }
  'derive'        { (_, TokLowerName [] "derive") }
  'do'            { (_, TokLowerName _ "do") }
  'else'          { (_, TokLowerName [] "else") }
  'false'         { (_, TokLowerName [] "false") }
  'forall'        { (_, TokLowerName [] "forall") }
  'forallu'       { (_, TokSymbol [] "∀") }
  'foreign'       { (_, TokLowerName [] "foreign") }
  'hiding'        { (_, TokLowerName [] "hiding") }
  'import'        { (_, TokLowerName [] "import") }
  'if'            { (_, TokLowerName [] "if") }
  'in'            { (_, TokLowerName [] "in") }
  'infix'         { (_, TokLowerName [] "infix") }
  'infixl'        { (_, TokLowerName [] "infixl") }
  'infixr'        { (_, TokLowerName [] "infixr") }
  'instance'      { (_, TokLowerName [] "instance") }
  'kind'          { (_, TokLowerName [] "kind") }
  'let'           { (_, TokLowerName [] "let") }
  'module'        { (_, TokLowerName [] "module") }
  'newtype'       { (_, TokLowerName [] "newtype") }
  'of'            { (_, TokLowerName [] "of") }
  'then'          { (_, TokLowerName [] "then") }
  'true'          { (_, TokLowerName [] "true") }
  'type'          { (_, TokLowerName [] "type") }
  'where'         { (_, TokLowerName [] "where") }
  IDENT           { (_, TokLowerName [] _) }
  QUAL_IDENT      { (_, TokLowerName _ _) }
  PROPER          { (_, TokUpperName [] _) }
  QUAL_PROPER     { (_, TokUpperName _ _) }
  SYMBOL          { (_, TokSymbol _ _) }
  LIT_HOLE        { (_, TokHole _) }
  LIT_CHAR        { (_, TokChar _ _) }
  LIT_STRING      { (_, TokString _ _) }
  LIT_RAW_STRING  { (_, TokRawString _) }
  LIT_INT         { (_, TokInt _ _) }
  LIT_NUMBER      { (_, TokNumber _ _) }

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
  | 'kind' { toIdent $1 }

var :: { Ident }
  : IDENT {% toVar $1 }
  | 'as' {% toVar $1 }
  | 'kind' {% toVar $1 }

symbol :: { Ident }
  : SYMBOL {% toSymbol $1 }
  | '<=' {% toSymbol $1 }
  | '-' {% toSymbol $1 }
  | '@' {% toSymbol $1 }
  | '#' {% toSymbol $1 }
  | ':' {% toSymbol $1 }
  | '..' {% toSymbol $1 }

label :: { Ident }
  : IDENT { toLabel $1 }
  | LIT_STRING { toLabel $1 }
  | LIT_RAW_STRING { toLabel $1 }
  | 'as' { toLabel $1 }
  | 'true' { toLabel $1 }
  | 'false' { toLabel $1 }
  | 'kind' { toLabel $1 }
  | labelKeyword { $1 }

labelKeyword :: { Ident }
  : 'ado' { toLabel $1 }
  | 'case' { toLabel $1 }
  | 'class' { toLabel $1 }
  | 'data' { toLabel $1 }
  | 'derive' { toLabel $1 }
  | 'do' { toLabel $1 }
  | 'else' { toLabel $1 }
  | 'forall' { toLabel $1 }
  | 'forallu' { toLabel $1 }
  | 'foreign' { toLabel $1 }
  | 'hiding' { toLabel $1 }
  | 'import' { toLabel $1 }
  | 'if' { toLabel $1 }
  | 'in' { toLabel $1 }
  | 'infix' { toLabel $1 }
  | 'infixl' { toLabel $1 }
  | 'infixr' { toLabel $1 }
  | 'instance' { toLabel $1 }
  | 'let' { toLabel $1 }
  | 'module' { toLabel $1 }
  | 'newtype' { toLabel $1 }
  | 'of' { toLabel $1 }
  | 'then' { toLabel $1 }
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
  | type1 symbol type2 { TypeOp () $1 $2 $3 }

type2 :: { Type () }
  : typeAtom { $1 }
  | type2 typeAtom { TypeApp () $1 $2 }

typeAtom :: { Type ()}
  : '_' { TypeWildcard () $1 }
  | var { TypeVar () $1 }
  | properIdent { TypeConstructor () $1 }
  | string { uncurry (TypeString ()) $1 }
  | hole { TypeHole () $1 }
  | '{' row '}' { TypeRecord () (Wrapped $1 $2 $3) }
  | '(' row ')' { TypeRow () (Wrapped $1 $2 $3) }
  | '(' '->' ')' { TypeArrName () (Wrapped $1 $2 $3) }
  | '(' symbol ')' { TypeOpName () (Wrapped $1 $2 $3) }
  | '(' type ')' { TypeParens () (Wrapped $1 $2 $3) }
  | '(' typeKindedAtom '::' kind ')' { TypeParens () (Wrapped $1 (TypeKinded () $2 $3 $4) $5) }

typeKindedAtom :: { Type () }
  : '_' { TypeWildcard () $1 }
  | properIdent { TypeConstructor () $1 }
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
  | expr0 symbol expr1 { ExprOp () $1 $2 $3 }

expr1 :: { Expr () }
  : expr2 { $1 }
  | expr1 '`' exprBacktick '`' expr2 { ExprInfix () $1 (Wrapped $2 $3 $4) $5 }

exprBacktick :: { Expr () }
  : expr2 { $1 }
  | exprBacktick symbol expr2 { ExprOp () $1 $2 $3 }

expr2 :: { Expr () }
  : expr3 { $1 }
  | '-' expr3 { ExprNegate () $1 $2 }

expr3 :: { Expr () }
  : expr4 { $1 }
  | expr3 expr4 { ExprApp () $1 $2 }

expr4 :: { Expr () }
  : expr5 { $1 }
  | 'if' expr 'then' expr 'else' expr { ExprIf () (IfThenElse $1 $2 $3 $4 $5 $6) }
  | 'do' '\{' manySep(doStatement, '\;') '\}' { ExprDo () (DoBlock $1 $3) }
  | 'ado' '\{' manySep(doStatement, '\;') '\}' 'in' expr { ExprAdo () (AdoBlock $1 $3 $5 $6) }
  | 'lambda' expr0 '->' expr {% do bs <- toBinderAtoms $2; pure $ ExprLambda () (Lambda $1 bs $3 $4) }
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
  : exprAtom { $1 }
  | exprAtom '.' sep(label, '.') { ExprRecordAccessor () (RecordAccessor $1 $2 $3) }
  | expr5 '{' '}' { ExprApp () $1 (ExprRecord () (Wrapped $2 Nothing $3)) }
  | expr5 '{' sep(recordUpdateOrLabel, ',') '}'
      {% toRecordFields $3 >>= \case
          Left xs -> pure $ ExprApp () $1 (ExprRecord () (Wrapped $2 (Just xs) $4))
          Right xs -> pure $ ExprRecordUpdate () $1 (Wrapped $2 xs $4)
      }

exprAtom :: { Expr () }
  : '_' { ExprSection () $1 }
  | hole { ExprHole () $1 }
  | ident { ExprIdent () $1 }
  | properIdent { ExprConstructor () $1 }
  | boolean { uncurry (ExprBoolean ()) $1 }
  | char { uncurry (ExprChar ()) $1 }
  | string { uncurry (ExprString ()) $1 }
  | number { uncurry (ExprNumber ()) $1 }
  | array { ExprArray () $1 }
  | record { ExprRecord () $1 }
  | '(' symbol ')' { ExprOpName () (Wrapped $1 $2 $3) }
  | '(' expr ')' { ExprParens () (Wrapped $1 $2 $3) }
  | error {%% recover ErrExpr unexpectedExpr }

array :: { Delimited (Expr ()) }
  : delim('[', expr, ',', ']') { $1 }

record :: { Delimited (RecordLabeled (Expr ())) }
  : delim('{', recordLabel, ',', '}') { $1 }

recordLabel :: { RecordLabeled (Expr ()) }
  : expr {% toRecordLabeled $1 }
  | labelKeyword ':' expr { RecordField $1 $2 $3 }

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
  | expr0 {%% recover ErrLetBinding (unexpectedLetBinding . (exprToken $1 :)) }

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
  | '(' symbol ')' { ExportOp () (Wrapped $1 $2 $3) }
  | proper { ExportType () $1 Nothing }
  | proper dataMembers { ExportType () $1 (Just $2) }
  | 'type' '(' symbol ')' { ExportTypeOp () $1 (Wrapped $2 $3 $4) }
  | 'class' proper { ExportClass () $1 $2 }
  | 'kind' proper { ExportKind () $1 $2 }
  | 'module' properIdent { ExportModule () $1 $2 }

dataMembers :: { Wrapped (Maybe (DataMembers ())) }
 : '(' ')' { Wrapped $1 Nothing $2 }
 | '(' '..' ')' { Wrapped $1 (Just (DataAll () $2)) $3 }
 | '(' sep(proper, ',') ')' { Wrapped $1 (Just (DataEnumerated () $2)) $3 }

importDecl :: { ImportDecl () }
  : 'import' properIdent imports { ImportDecl () $1 $2 $3 Nothing }
  | 'import' properIdent imports 'as' proper { ImportDecl () $1 $2 $3 (Just ($4, $5)) }

imports :: { Maybe (Maybe SourceToken, DelimitedNonEmpty (Import ())) }
  : {- empty -} { Nothing }
  | '(' sep(import, ',') ')' { Just (Nothing, Wrapped $1 $2 $3) }
  | 'hiding' '(' sep(import, ',') ')' { Just (Just $1, Wrapped $2 $3 $4) }

import :: { Import () }
  : var { ImportValue () $1 }
  | '(' symbol ')' { ImportOp () (Wrapped $1 $2 $3) }
  | proper { ImportType () $1 Nothing }
  | proper dataMembers { ImportType () $1 (Just $2) }
  | 'type' '(' symbol ')' { ImportTypeOp () $1 (Wrapped $2 $3 $4) }
  | 'class' proper { ImportClass () $1 $2 }
  | 'kind' proper { ImportKind () $1 $2 }

decl :: { Declaration () }
  : dataHead { DeclData () $1 Nothing }
  | dataHead '=' sep(dataCtor, '|') { DeclData () $1 (Just ($2, $3)) }
  | typeHead '=' type { DeclType () $1 $2 $3 }
  | newtypeHead '=' properIdent typeAtom { DeclNewtype () $1 $2 $3 $4 }
  | classHead { DeclClass () $1 Nothing }
  | classHead 'where' '\{' manySep(classMember, '\;') '\}'
      { DeclClass () $1 (Just ($2, $4)) }
  | instHead { DeclInstanceChain () (Separated (Instance $1 Nothing) []) }
  | instHead 'where' '\{' manySep(instBinding, '\;') '\}'
      { DeclInstanceChain () (Separated (Instance $1 (Just ($2, $4))) []) }
  | 'derive' instHead { DeclDerive () $1 Nothing $2 }
  | 'derive' 'newtype' instHead { DeclDerive () $1 (Just $2) $3 }
  | ident '::' type { DeclSignature () (Labeled $1 $2 $3) }
  | expr0 guarded('=') {% toDecl DeclValue unexpectedDecl $1 $2 }
  | fixity { DeclFixity () $1 }
  | 'foreign' 'import' foreign { DeclForeign () $1 $2 $3 }
  | expr0 error {%% recover ErrExprInDecl (unexpectedDecl . (exprToken $1 :)) }
  | error {%% recover ErrDecl unexpectedDecl }

dataHead :: { DataHead () }
  : 'data' properIdent manyOrEmpty(typeVarBinding) { DataHead $1 $2 $3 }

typeHead :: { DataHead () }
  : 'type' properIdent manyOrEmpty(typeVarBinding) { DataHead $1 $2 $3 }

newtypeHead :: { DataHead () }
  : 'newtype' properIdent manyOrEmpty(typeVarBinding) { DataHead $1 $2 $3 }

dataCtor :: { DataCtor () }
  : properIdent manyOrEmpty(type) { DataCtor () $1 $2 }

classHead :: { ClassHead () }
  : 'class' classNameAndVars fundeps { ClassHead $1 Nothing (fst $2) (snd $2) $3 }
  -- We need to inline constraints due to the reduce/reduce conflict between
  -- the class name and vars and constraint syntax.
  | 'class' classNameAndVars '<=' classNameAndVars fundeps
      { ClassHead $1
          (Just (One (foldl' (TypeApp ()) (TypeVar () (fst $2)) (fmap varToType (snd $2))), $3))
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
  : ident '::' type { Labeled $1 $2 $3 }

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
  | properIdent many(typeAtom) { foldl' (TypeApp ()) (TypeConstructor () $1) $2 }

instBinding :: { InstanceBinding () }
  : ident '::' type { InstanceBindingSignature () (Labeled $1 $2 $3) }
  | expr0 guarded('=') {% toDecl InstanceBindingName unexpectedInstBinding $1 $2 }

fixity :: { FixityFields () }
  : infix int var 'as' symbol { FixityFields $1 $2 Nothing $3 $4 $5 }
  | infix int proper 'as' symbol { FixityFields $1 $2 Nothing $3 $4 $5 }
  | infix int 'type' proper 'as' symbol { FixityFields $1 $2 (Just $3) $4 $5 $6 }

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
