module Language.PureScript.CST.Positions where

import Prelude

import Data.Foldable (foldl')
import Data.Text (Text)
import Data.Void (Void)
import qualified Data.Text as Text
import Language.PureScript.CST.Types

advanceToken :: SourcePos -> Token -> SourcePos
advanceToken pos = applyDelta pos . tokenDelta

advanceLeading :: SourcePos -> [Comment LineFeed] -> SourcePos
advanceLeading pos = foldl' (\a -> applyDelta a . commentDelta lineDelta) pos

advanceTrailing :: SourcePos -> [Comment Void] -> SourcePos
advanceTrailing pos = foldl' (\a -> applyDelta a . commentDelta (const (0, 0))) pos

tokenDelta :: Token -> (Int, Int)
tokenDelta = \case
  TokLeftParen             -> (0, 1)
  TokRightParen            -> (0, 1)
  TokLeftBrace             -> (0, 1)
  TokRightBrace            -> (0, 1)
  TokLeftSquare            -> (0, 1)
  TokRightSquare           -> (0, 1)
  TokLeftArrow ASCII       -> (0, 2)
  TokLeftArrow Unicode     -> (0, 1)
  TokRightArrow ASCII      -> (0, 2)
  TokRightArrow Unicode    -> (0, 1)
  TokRightFatArrow ASCII   -> (0, 2)
  TokRightFatArrow Unicode -> (0, 1)
  TokDoubleColon ASCII     -> (0, 2)
  TokDoubleColon Unicode   -> (0, 1)
  TokForall ASCII          -> (0, 6)
  TokForall Unicode        -> (0, 1)
  TokEquals                -> (0, 1)
  TokPipe                  -> (0, 1)
  TokTick                  -> (0, 1)
  TokDot                   -> (0, 1)
  TokComma                 -> (0, 1)
  TokUnderscore            -> (0, 1)
  TokBackslash             -> (0, 1)
  TokLowerName qual name   -> (0, qualDelta qual + Text.length name)
  TokUpperName qual name   -> (0, qualDelta qual + Text.length name)
  TokSymbolName qual sym   -> (0, qualDelta qual + Text.length sym + 2)
  TokSymbol qual sym       -> (0, qualDelta qual + Text.length sym)
  TokSymbolArr Unicode     -> (0, 3)
  TokSymbolArr ASCII       -> (0, 4)
  TokHole hole             -> (0, Text.length hole + 1)
  TokChar raw _            -> (0, Text.length raw + 2)
  TokInt raw _             -> (0, Text.length raw)
  TokNumber raw _          -> (0, Text.length raw)
  TokString raw _          -> multiLine 1 $ textDelta raw
  TokRawString raw         -> multiLine 3 $ textDelta raw
  TokLayoutStart           -> (0, 0)
  TokLayoutSep             -> (0, 0)
  TokLayoutEnd             -> (0, 0)
  TokEof                   -> (0, 0)

qualDelta :: [Text] -> Int
qualDelta = foldr ((+) . (+ 1) . Text.length) 0

multiLine :: Int -> (Int, Int) -> (Int, Int)
multiLine n (0, c) = (0, c + n + n)
multiLine n (l, c) = (l, c + n)

commentDelta :: (a -> (Int, Int)) -> Comment a -> (Int, Int)
commentDelta k = \case
  Comment raw -> textDelta raw
  Space n -> (0, n)
  Line a -> k a

lineDelta :: LineFeed -> (Int, Int)
lineDelta _ = (1, 1)

textDelta :: Text -> (Int, Int)
textDelta = Text.foldl' go (0, 0)
  where
  go (!l, !c) = \case
    '\n' -> (l + 1, 1)
    _    -> (l, c + 1)

applyDelta :: SourcePos -> (Int, Int) -> SourcePos
applyDelta (SourcePos l c) = \case
  (0, n) -> SourcePos l (c + n)
  (k, d) -> SourcePos (l + k) d

sepLast :: Separated a -> a
sepLast (Separated hd []) = hd
sepLast (Separated _ tl) = snd $ last tl

type TokenRange = (SourceToken, SourceToken)

toSourceRange :: TokenRange -> SourceRange
toSourceRange (a, b) = widen (srcRange a) (srcRange b)

widen :: SourceRange -> SourceRange -> SourceRange
widen (SourceRange s1 _) (SourceRange _ e2) = SourceRange s1 e2

srcRange :: SourceToken -> SourceRange
srcRange = tokRange . tokAnn

identRange :: Ident -> TokenRange
identRange a = (identTok a, identTok a)

wrappedRange :: Wrapped a -> TokenRange
wrappedRange (Wrapped { wrpOpen, wrpClose }) = (wrpOpen, wrpClose)

moduleRange :: Module a -> TokenRange
moduleRange (Module { modKeyword, modWhere, modImports, modDecls }) =
  case (modImports, modDecls) of
    ([], []) -> (modKeyword, modWhere)
    (is, []) -> (modKeyword, snd . importDeclRange $ last is)
    (_,  ds) -> (modKeyword, snd . declRange $ last ds)

exportRange :: Export a -> TokenRange
exportRange = \case
  ExportValue _ a -> identRange a
  ExportOp _ a -> identRange a
  ExportType _ a b
    | Just b' <- b -> (identTok a, snd $ dataMembersRange b')
    | otherwise -> identRange a
  ExportTypeOp _ a b -> (a, identTok b)
  ExportClass _ a b -> (a, identTok b)
  ExportKind _ a b -> (a, identTok b)
  ExportModule _ a b -> (a, identTok b)

importDeclRange :: ImportDecl a -> TokenRange
importDeclRange (ImportDecl { impKeyword, impModule, impNames, impQual })
  | Just (_, ident) <- impQual = (impKeyword, identTok ident)
  | Just (_, imports) <- impNames = (impKeyword, wrpClose imports)
  | otherwise = (impKeyword, identTok impModule)

importRange :: Import a -> TokenRange
importRange = \case
  ImportValue _ a -> identRange a
  ImportOp _ a -> identRange a
  ImportType _ a b
    | Just b' <- b -> (identTok a, snd $ dataMembersRange b')
    | otherwise -> identRange a
  ImportTypeOp _ a b -> (a, identTok b)
  ImportClass _ a b -> (a, identTok b)
  ImportKind _ a b -> (a, identTok b)

dataMembersRange :: DataMembers a -> TokenRange
dataMembersRange = \case
  DataAll _ a -> (a, a)
  DataEnumerated _ (Wrapped a _ b) -> (a, b)

declRange :: Declaration a -> TokenRange
declRange = \case
  DeclData _ hd ctors
    | Just (_, cs) <- ctors -> (fst start, snd . dataCtorRange $ sepLast cs)
    | otherwise -> start
    where start = dataHeadRange hd
  DeclType _ a _ b -> (fst $ dataHeadRange a,  snd $ typeRange b)
  DeclNewtype _ a _ _ b -> (fst $ dataHeadRange a, snd $ typeRange b)
  DeclClass _ hd body
    | Just (t, []) <- body -> (fst start, t)
    | Just (_, ts) <- body -> (fst start, snd . typeRange . lblValue $ last ts)
    | otherwise -> start
    where start = classHeadRange hd
  DeclInstanceChain _ a -> (fst . instanceRange $ sepHead a, snd . instanceRange $ sepLast a)
  DeclDerive _ a _ b -> (a, snd $ instanceHeadRange b)
  DeclSignature _ (Labeled a _ b) -> (identTok a, snd $ typeRange b)
  DeclValue _ a -> valueBindingFieldsRange a
  DeclFixity _ (FixityFields a _ _ _ _ b) -> (a, identTok b)
  DeclForeign _ a _ b -> (a, snd $ foreignRange b)

dataHeadRange :: DataHead a -> TokenRange
dataHeadRange (DataHead kw name vars)
  | [] <- vars = (kw, identTok name)
  | otherwise = (kw, snd . typeVarBindingRange $ last vars)

dataCtorRange :: DataCtor a -> TokenRange
dataCtorRange (DataCtor _ name fields)
  | [] <- fields = identRange name
  | otherwise = (identTok name, snd . typeRange $ last fields)

classHeadRange :: ClassHead a -> TokenRange
classHeadRange (ClassHead kw _ name vars fdeps)
  | Just (_, fs) <- fdeps = (kw, snd .classFundepRange $ sepLast fs)
  | [] <- vars = (kw, snd $ identRange name)
  | otherwise = (kw, snd . typeVarBindingRange $ last vars)

classFundepRange :: ClassFundep -> TokenRange
classFundepRange = \case
  ClassFundep [] arr [] -> (arr, arr)
  ClassFundep as arr [] -> (identTok $ head as, arr)
  ClassFundep [] arr bs -> (arr, identTok $ last bs)
  ClassFundep as _   bs -> (identTok $ head as, identTok $ last bs)

instanceRange :: Instance a -> TokenRange
instanceRange (Instance hd bd)
  | Just (t, []) <- bd = (fst start, t)
  | Just (_, ts) <- bd = (fst start, snd . instanceBindingRange $ last ts)
  | otherwise = start
  where start = instanceHeadRange hd

instanceHeadRange :: InstanceHead a -> TokenRange
instanceHeadRange (InstanceHead kw _ _ _ cls types)
  | [] <- types = (kw, snd . typeRange $ last types)
  | otherwise = (kw, identTok cls)

instanceBindingRange :: InstanceBinding a -> TokenRange
instanceBindingRange = \case
  InstanceBindingSignature _ (Labeled a _ b) -> (identTok a, snd $ typeRange b)
  InstanceBindingName _ a -> valueBindingFieldsRange a

foreignRange :: Foreign a -> TokenRange
foreignRange = \case
  ForeignValue (Labeled a _ b) -> (identTok a, snd $ typeRange b)
  ForeignData a (Labeled _ _ b) -> (a, snd $ kindRange b)
  ForeignKind a b -> (a, identTok b)

valueBindingFieldsRange :: ValueBindingFields a -> TokenRange
valueBindingFieldsRange (ValueBindingFields a _ b) = (identTok a, snd $ guardedRange b)

guardedRange :: Guarded a -> TokenRange
guardedRange = \case
  Unconditional a b -> (a, snd $ exprRange b)
  Guarded as -> (fst . guardedExprRange $ head as, snd . guardedExprRange $ last as)

guardedExprRange :: GuardedExpr a -> TokenRange
guardedExprRange (GuardedExpr a _ _ b) = (a, snd $ exprRange b)

kindRange :: Kind a -> TokenRange
kindRange = \case
  KindName _ a -> identRange a
  KindArr _ a _ b -> (fst $ kindRange a, snd $ kindRange b)
  KindRow _ a b -> (a, snd $ kindRange b)
  KindParens _ a -> wrappedRange a

typeRange :: Type a -> TokenRange
typeRange = \case
  TypeVar _ a -> identRange a
  TypeConstructor _ a -> identRange a
  TypeWildcard _ a -> (a, a)
  TypeHole _ a -> identRange a
  TypeString _ a _ -> (a, a)
  TypeRow _ a -> wrappedRange a
  TypeRecord _ a -> wrappedRange a
  TypeForall _ a _ _ b -> (a, snd $ typeRange b)
  TypeKinded _ a _ b -> (fst $ typeRange a, snd $ kindRange b)
  TypeApp _ a b -> (fst $ typeRange a, snd $ typeRange b)
  TypeOp _ a _ b -> (fst $ typeRange a, snd $ typeRange b)
  TypeOpName _ a -> identRange a
  TypeArr _ a _ b -> (fst $ typeRange a, snd $ typeRange b)
  TypeArrName _ a -> (a, a)
  TypeConstrained _ a _ b -> (fst $ typeRange a, snd $ typeRange b)
  TypeParens _ a -> wrappedRange a

typeVarBindingRange :: TypeVarBinding a -> TokenRange
typeVarBindingRange = \case
  TypeVarKinded a -> wrappedRange a
  TypeVarName a -> identRange a

exprRange :: Expr a -> TokenRange
exprRange = \case
  ExprHole _ a -> identRange a
  ExprSection _ a -> (a, a)
  ExprIdent _ a -> identRange a
  ExprConstructor _ a -> identRange a
  ExprBoolean _ a _ -> (a, a)
  ExprChar _ a _ -> (a, a)
  ExprString _ a _ -> (a, a)
  ExprNumber _ a _ -> (a, a)
  ExprArray _ a -> wrappedRange a
  ExprRecord _ a -> wrappedRange a
  ExprParens _ a -> wrappedRange a
  ExprTyped _ a _ b -> (fst $ exprRange a, snd $ typeRange b)
  ExprInfix _ a _ b -> (fst $ exprRange a, snd $ exprRange b)
  ExprOp _ a _ b -> (fst $ exprRange a, snd $ exprRange b)
  ExprOpName _ a -> identRange a
  ExprNegate _ a b -> (a, snd $ exprRange b)
  ExprRecordAccessor _ (RecordAccessor a _ b) -> (fst $ exprRange a, identTok $ sepLast b)
  ExprRecordUpdate _ a b -> (fst $ exprRange a, snd $ wrappedRange b)
  ExprApp _ a b -> (fst $ exprRange a, snd $ exprRange b)
  ExprLambda _ (Lambda a _ _ b) -> (a, snd $ exprRange b)
  ExprIf _ (IfThenElse a _ _ _ _ b) -> (a, snd $ exprRange b)
  ExprCase _ (CaseOf a _ b c)
    | [] <- c -> (a, b)
    | otherwise -> (a, snd . guardedRange . snd $ last c)
  ExprLet _ (LetIn a _ _ b) -> (a, snd $ exprRange b)
  ExprWhere _ (Where a b c)
    | [] <- c -> (start, b)
    | otherwise -> (start, snd . letBindingRange $ last c)
    where start = fst $ exprRange a
  ExprDo _ (DoBlock a b) -> (a,  snd . doStatementRange $ last b)
  ExprAdo _ (AdoBlock a _ _ b) -> (a, snd $ exprRange b)

letBindingRange :: LetBinding a -> TokenRange
letBindingRange = \case
  LetBindingSignature _ (Labeled a _ b) -> (identTok a, snd $ typeRange b)
  LetBindingName _ a -> valueBindingFieldsRange a
  LetBindingPattern _ a _ b -> (fst $ binderRange a, snd $ exprRange b)

doStatementRange :: DoStatement a -> TokenRange
doStatementRange = \case
  DoLet a bs -> (a, snd . letBindingRange $ last bs)
  DoDiscard a -> exprRange a
  DoBind a _ b -> (fst $ binderRange a, snd $ exprRange b)

binderRange :: Binder a -> TokenRange
binderRange = \case
  BinderWildcard _ a -> (a, a)
  BinderVar _ a -> identRange a
  BinderNamed _ a _ b -> (identTok a, snd $ binderRange b)
  BinderConstructor _ a bs
    | [] <- bs -> identRange a
    | otherwise -> (identTok a, snd . binderRange $ last bs)
  BinderBoolean _ a _ -> (a, a)
  BinderChar _ a _ -> (a, a)
  BinderString _ a _ -> (a, a)
  BinderNumber _ a b _
    | Just a' <- a -> (a', b)
    | otherwise -> (b, b)
  BinderArray _ a -> wrappedRange a
  BinderRecord _ a -> wrappedRange a
  BinderParens _ a -> wrappedRange a
  BinderTyped _ a _ b -> (fst $ binderRange a, snd $ typeRange b)
  BinderOp _ a _ b -> (fst $ binderRange a, snd $ binderRange b)

recordUpdateRange :: RecordUpdate a -> TokenRange
recordUpdateRange = \case
  RecordUpdateLeaf a _ b -> (identTok a, snd $ exprRange b)
  RecordUpdateBranch a (Wrapped _ _ b) -> (identTok a, b)
