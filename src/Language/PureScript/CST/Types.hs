module Language.PureScript.CST.Types where

import Prelude

import Data.Text (Text)
import Data.Void (Void)
import GHC.Generics (Generic)

data SourcePos = SourcePos
  { srcLine :: !Int
  , srcColumn :: !Int
  } deriving (Show, Eq, Ord, Generic)

data SourceRange = SourceRange
  { srcStart :: !SourcePos
  , srcEnd :: !SourcePos
  } deriving (Show, Eq, Ord, Generic)

data Comment l
  = Comment !Text
  | Space Int
  | Line l
  deriving (Show, Eq, Ord, Generic, Functor)

data LineFeed = LF | CRLF
  deriving (Show, Eq, Ord, Generic)

data TokenAnn = TokenAnn
  { tokRange :: SourceRange
  , tokLeadingComments :: ![Comment LineFeed]
  , tokTrailingComments :: ![Comment Void]
  } deriving (Show, Eq, Ord, Generic)

data SourceStyle = ASCII | Unicode
  deriving (Show, Eq, Ord, Generic)

data Token
  = TokLeftParen
  | TokRightParen
  | TokLeftBrace
  | TokRightBrace
  | TokLeftSquare
  | TokRightSquare
  | TokLeftArrow SourceStyle
  | TokRightArrow SourceStyle
  | TokRightFatArrow SourceStyle
  | TokDoubleColon SourceStyle
  | TokEquals
  | TokPipe
  | TokTick
  | TokDot
  | TokComma
  | TokUnderscore
  | TokLowerName [Text] Text
  | TokUpperName [Text] Text
  | TokSymbol [Text] Text
  | TokHole Text
  | TokChar Text Char
  | TokString Text Text
  | TokRawString Text
  | TokInt Text Integer
  | TokNumber Text Double
  | TokLayoutStart
  | TokLayoutSep
  | TokLayoutEnd
  deriving (Show, Eq, Ord, Generic)

type SourceToken = (TokenAnn, Token)

data Ident = Ident
  { identTok :: SourceToken
  , identQual :: [Text]
  , identName :: Text
  } deriving (Show, Eq, Ord, Generic)

data Wrapped a = Wrapped
  { wrpOpen :: SourceToken
  , wrpValue :: a
  , wrpClose :: SourceToken
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

data Separated a = Separated
  { sepHead :: a
  , sepTail :: [(SourceToken, a)]
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

data Labeled a = Labeled
  { lblLabel :: Ident
  , lblSep :: SourceToken
  , lblValue  :: a
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

type Delimited a = Wrapped (Maybe (Separated a))
type DelimitedNonEmpty a = Wrapped (Separated a)

data OneOrDelimited a
  = One a
  | Many (DelimitedNonEmpty a)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

data Kind a
  = KindName a Ident
  | KindArr a (Kind a) SourceToken (Kind a)
  | KindRow a SourceToken (Kind a)
  | KindParens a (Wrapped (Kind a))
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

data Type a
  = TypeVar a Ident
  | TypeConstructor a Ident
  | TypeWildcard a SourceToken
  | TypeHole a Ident
  | TypeString a SourceToken Text
  | TypeRow a (Wrapped (Row a))
  | TypeRecord a (Wrapped (Row a))
  | TypeForall a SourceToken [TypeVarBinding a] SourceToken (Type a)
  | TypeKinded a (Type a) SourceToken (Kind a)
  | TypeApp a (Type a) (Type a)
  | TypeOp a (Type a) Ident (Type a)
  | TypeArr a (Type a) SourceToken (Type a)
  | TypeConstrained a (Type a) SourceToken (Type a)
  | TypeParens a (Wrapped (Type a))
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

data TypeVarBinding a
  = TypeVarKinded (Wrapped (Labeled (Kind a)))
  | TypeVarName Ident
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

data Row a = Row
  { rowLabels :: Maybe (Separated (Labeled (Type a)))
  , rowTail :: Maybe (SourceToken, Type a)
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

data ModuleDecl a = ModuleDecl
  { modAnn :: a
  , modKeyword :: SourceToken
  , modNamespace :: Ident
  , modExports :: (DelimitedNonEmpty (Export a))
  , modWhere :: SourceToken
  , modImports :: [ImportFields a]
  , modDecls :: [Declaration a]
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

data Export a
  = ExportValue a Ident
  | ExportOp a (Wrapped Ident)
  | ExportType a SourceToken (Maybe (Wrapped (Maybe (DataMembers a))))
  | ExportTypeOp a SourceToken (Wrapped Ident)
  | ExportClass a SourceToken Ident
  | ExportKind a SourceToken Ident
  | ExportModule a SourceToken Ident
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

data DataMembers a
  = DataAll a SourceToken SourceToken
  | DataEnumerated a (Separated Ident)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

data Declaration a
  = DeclData (DeclDataHead a) (Maybe (SourceToken, Separated (DeclDataCtor a)))
  | DeclType (DeclDataHead a) SourceToken (Type a)
  | DeclClass a (DeclClassHead a) (Maybe (SourceToken, [Labeled (Type a)]))
  | DeclInstance a (DeclInstanceHead a) (Maybe (SourceToken, [DeclValueFields a]))
  | DeclValue a (DeclValueFields a)
  | DeclFixity a (DeclFixityFields a)
  | DeclForeign a SourceToken SourceToken (Foreign a)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

data ImportFields a = ImportFields
  { impAnn :: a
  , impKeyword :: SourceToken
  , impModule :: Ident
  , impNames :: Maybe (Delimited (Import a))
  , impQualification :: Maybe (SourceToken, Ident)
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

data Import a
  = ImportValue a Ident
  | ImportOp a (Wrapped Ident)
  | ImportType a SourceToken (Maybe (Wrapped (Maybe (DataMembers a))))
  | ImportTypeOp a SourceToken (Wrapped Ident)
  | ImportClass a SourceToken Ident
  | ImportKind a SourceToken Ident
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

data DeclDataHead a = DeclDataHead
  { dataHdAnn :: a
  , dataHdName :: Ident
  , dataHdVars :: [TypeVarBinding a]
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

data DeclDataCtor a = DeclDataCtor
  { dataCtorAnn :: a
  , dataCtorName :: Ident
  , dataCtorFields :: [Type a]
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

data DeclClassHead a = DeclClassHead
  { clsKeyword :: SourceToken
  , clsSuper :: Maybe (OneOrDelimited (Type a), SourceToken)
  , clsName :: Ident
  , clsVars :: [TypeVarBinding a]
  , clsFundeps :: Maybe (SourceToken, Separated (DeclClassFundep a))
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

data DeclClassFundep a = DeclClassFundep
  { fndLhs :: [Ident]
  , fndArr :: SourceToken
  , fndRhs :: [Ident]
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

data DeclInstanceHead a = DeclInstanceHead
  { instKeyword :: SourceToken
  , instConstraints :: Maybe (OneOrDelimited (Type a), SourceToken)
  , instName :: Ident
  , instTypes :: [Type a]
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

data DeclValueFields a = DeclValueFields
  { valSig :: Maybe (Labeled (Type a))
  , valDef:: ValueBinding a
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

data DeclFixityFields a = DeclFixityFields
  { fxtKeyword :: SourceToken
  , fxtNum :: (SourceToken, Integer)
  , fxtType :: Maybe SourceToken
  , fxtAs :: SourceToken
  , fxtSymbol :: Ident
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

data ValueBinding a = ValueBinding
  { valName :: Ident
  , valBinders :: [Binder a]
  , valGuarded :: Guarded a
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

data Guarded a
  = Unconditional SourceToken (Expr a)
  | Guarded [GuardedExpr a]
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

data GuardedExpr a = GuardedExpr
  { grdBar :: SourceToken
  , grdPatterns :: Separated (PatternGuard a)
  , grdSep :: SourceToken
  , grdExpr :: Expr a
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

data PatternGuard a = Guard
  { patBinder :: Maybe (Binder a, SourceToken)
  , patExpr :: Expr a
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

data Foreign a
  = ForeignValue (Labeled (Type a))
  | ForeignData SourceToken (Labeled (Kind a))
  | ForeignKind SourceToken Ident
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

data Expr a
  = ExprHole a Ident
  | ExprSection a SourceToken
  | ExprVar a Ident
  | ExprConstructor a Ident
  | ExprChar a SourceToken Char
  | ExprString a SourceToken Text
  | ExprNumber a SourceToken (Either Integer Double)
  | ExprArray a (Delimited (Expr a))
  | ExprRecord a (Delimited (RecordLabeled (Expr a)))
  | ExprParens a (Wrapped (Expr a))
  | ExprTyped a (Expr a) SourceToken (Type a)
  | ExprOp a (Expr a) Ident (Expr a)
  | ExprUnaryMinus a SourceToken (Expr a)
  | ExprRecordAccessor a (RecordAccessor a)
  | ExprRecordUpdate a (Expr a) (DelimitedNonEmpty (RecordUpdate a))
  | ExprApp a (Expr a) (Expr a)
  | ExprInfix a (Infix a)
  | ExprLambda a (Lambda a)
  | ExprIf a (IfThenElse a)
  | ExprCase a (CaseOf a)
  | ExprLet a (LetIn a)
  | ExprWhere a (Where a)
  | ExprDo a (DoBlock a)
  | ExprAdo a (AdoBlock a)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

data RecordLabeled a
  = RecordPun Ident
  | RecordField Ident SourceToken a
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

data RecordUpdate a
  = RecordUpdateLeaf Ident SourceToken (Expr a)
  | RecordUpdateBranch Ident (DelimitedNonEmpty (RecordUpdate a))
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

data RecordAccessor a = RecordAccessor
  { recExpr :: Expr a
  , recDot :: SourceToken
  , recField :: Ident
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

data Infix a = Infix
  { ifxLhs :: Expr a
  , ifxOpen :: SourceToken
  , ifxExpr :: Expr a
  , ifxClose :: SourceToken
  , ifxRhs :: Expr a
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

data Lambda a = Lambda
  { lmbSymbol :: SourceToken
  , lmbBinders :: [Binder a]
  , lmbArr :: SourceToken
  , lmbBody :: Expr a
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

data IfThenElse a = IfThenElse
  { iteIf :: SourceToken
  , iteHead :: Expr a
  , iteThen :: SourceToken
  , iteTrue :: Expr a
  , iteElse :: SourceToken
  , iteFalse :: Expr a
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

data CaseOf a = CaseOf
  { caseKeyword :: SourceToken
  , caseHead :: Separated (Expr a)
  , caseOf :: SourceToken
  , caseBranches :: [(Separated (Binder a), Guarded a)]
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

data LetIn a = LetIn
  { letKeyword :: SourceToken
  , letBindings :: [DeclValueFields a]
  , letIn :: SourceToken
  , letBody :: Expr a
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

data Where a = Where
  { whereBody :: Expr a
  , whereKeyword :: SourceToken
  , whereBindings :: [DeclValueFields a]
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

data DoBlock a = DoBlock
  { doKeyword :: SourceToken
  , doStatements :: [DoStatement a]
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

data DoStatement a
  = DoLet [DeclValueFields a]
  | DoDiscard (Expr a)
  | DoBind (Binder a) SourceToken (Expr a)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

data AdoBlock a = AdoBlock
  { adoKeyword :: SourceToken
  , adoStatements :: [DoStatement a]
  , adoIn :: SourceToken
  , adoResult :: Expr a
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

data Binder a
  = BinderWildcard a SourceToken
  | BinderVar a Ident
  | BinderNamed a Ident SourceToken (Binder a)
  | BinderConstructor a Ident [Binder a]
  | BinderChar a SourceToken Char
  | BinderString a SourceToken Text
  | BinderInt a SourceToken Integer
  | BinderNumber a SourceToken Double
  | BinderArray a (Delimited (Binder a))
  | BinderRecord a (Delimited (RecordLabeled (Binder a)))
  | BinderParens a (Wrapped (Binder a))
  | BinderTyped a (Binder a) SourceToken (Type a)
  | BinderOp a (Binder a) Ident (Binder a)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)
