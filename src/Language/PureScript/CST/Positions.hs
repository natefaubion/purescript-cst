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
  TokSymbol qual sym       -> (0, qualDelta qual + Text.length sym)
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

widen :: SourceRange -> SourceRange -> SourceRange
widen (SourceRange s1 _) (SourceRange _ e2) = SourceRange s1 e2

srcRange :: SourceToken -> SourceRange
srcRange = tokRange . fst

identRange :: Ident -> SourceRange
identRange = srcRange . identTok

wrappedRange :: Wrapped a -> SourceRange
wrappedRange (Wrapped { wrpOpen, wrpClose }) =
  widen (srcRange wrpOpen) (srcRange wrpClose)

moduleRange :: Module a -> SourceRange
moduleRange (Module { modKeyword, modWhere, modImports, modDecls }) =
  case (modImports, modDecls) of
    ([], []) -> widen start $ srcRange modWhere
    (is, []) -> widen start . importDeclRange $ last is
    (_,  ds) -> widen start . declRange $ last ds
  where start = srcRange modKeyword

importDeclRange :: ImportDecl a -> SourceRange
importDeclRange (ImportDecl { impKeyword, impModule, impNames, impQual })
  | Just (_, ident) <- impQual = widen start $ identRange ident
  | Just (_, imports) <- impNames = widen start $ wrappedRange imports
  | otherwise = widen start $ identRange impModule
  where start = srcRange impKeyword

declRange :: Declaration a -> SourceRange
declRange = \case
  DeclData _ hd ctors
    | Just (_, cs) <- ctors -> widen start $ dataCtorRange $ sepLast cs
    | otherwise -> start
    where start = dataHeadRange hd
  DeclType _ a _ b -> widen (dataHeadRange a) (typeRange b)
  DeclNewtype _ a _ _ b -> widen (dataHeadRange a) (typeRange b)
  DeclClass _ hd body
    | Just (t, []) <- body -> widen start $ srcRange t
    | Just (_, ts) <- body -> widen start $ typeRange $ lblValue $ last ts
    | otherwise -> start
    where start = classHeadRange hd
  DeclInstanceChain _ a -> widen (instanceRange $ sepHead a) (instanceRange $ sepLast a)
  DeclDerive _ a _ b -> widen (srcRange a) (instanceHeadRange b)
  DeclSignature _ (Labeled a _ b) -> widen (identRange a) (typeRange b)
  DeclValue _ a -> valueBindingFieldsRange a
  DeclFixity _ (FixityFields a _ _ _ _ b) -> widen (srcRange a) (identRange b)
  DeclForeign _ a _ b -> widen (srcRange a) (foreignRange b)

dataHeadRange :: DataHead a -> SourceRange
dataHeadRange (DataHead kw name vars)
  | [] <- vars = widen start $ identRange name
  | otherwise = widen start $ typeVarBindingRange $ last vars
  where start = srcRange kw

dataCtorRange :: DataCtor a -> SourceRange
dataCtorRange (DataCtor _ name fields)
  | [] <- fields = start
  | otherwise = widen start $ typeRange $ last fields
  where start = identRange name

classHeadRange :: ClassHead a -> SourceRange
classHeadRange (ClassHead kw _ name vars fdeps)
  | Just (_, fs) <- fdeps = widen start $ classFundepRange $ sepLast fs
  | [] <- vars = widen start $ identRange name
  | otherwise = widen start $ typeVarBindingRange $ last vars
  where start = srcRange kw

classFundepRange :: ClassFundep -> SourceRange
classFundepRange = \case
  ClassFundep [] arr [] -> srcRange arr
  ClassFundep as arr [] -> widen (identRange $ head as) (srcRange arr)
  ClassFundep [] arr bs -> widen (srcRange arr) (identRange $ last bs)
  ClassFundep as _   bs -> widen (identRange $ head as) (identRange $ last bs)

instanceRange :: Instance a -> SourceRange
instanceRange (Instance hd bd)
  | Just (t, []) <- bd = widen start $ srcRange t
  | Just (_, ts) <- bd = widen start $ instanceBindingRange $ last ts
  | otherwise = start
  where start = instanceHeadRange hd

instanceHeadRange :: InstanceHead a -> SourceRange
instanceHeadRange (InstanceHead kw _ _ _ cls types)
  | [] <- types = widen start $ typeRange $ last types
  | otherwise = widen start $ identRange cls
  where start = srcRange kw

instanceBindingRange :: InstanceBinding a -> SourceRange
instanceBindingRange = \case
  InstanceBindingSignature _ (Labeled a _ b) -> widen (identRange a) (typeRange b)
  InstanceBindingName _ a -> valueBindingFieldsRange a

foreignRange :: Foreign a -> SourceRange
foreignRange = \case
  ForeignValue (Labeled a _ b) -> widen (identRange a) (typeRange b)
  ForeignData a (Labeled _ _ b) -> widen (srcRange a) (kindRange b)
  ForeignKind a b -> widen (srcRange a) (identRange b)

valueBindingFieldsRange :: ValueBindingFields a -> SourceRange
valueBindingFieldsRange (ValueBindingFields a _ b) = widen (identRange a) (guardedRange b)

guardedRange :: Guarded a -> SourceRange
guardedRange = \case
  Unconditional a b -> widen (srcRange a) (exprRange b)
  Guarded as -> widen (guardedExprRange $ head as) (guardedExprRange $ last as)

guardedExprRange :: GuardedExpr a -> SourceRange
guardedExprRange (GuardedExpr a _ _ b) = widen (srcRange a) (exprRange b)

kindRange :: Kind a -> SourceRange
kindRange = \case
  KindName _ a -> identRange a
  KindArr _ a _ b -> widen (kindRange a) (kindRange b)
  KindRow _ a b -> widen (srcRange a) (kindRange b)
  KindParens _ a -> wrappedRange a

typeRange :: Type a -> SourceRange
typeRange = \case
  TypeVar _ a -> identRange a
  TypeConstructor _ a -> identRange a
  TypeWildcard _ a -> srcRange a
  TypeHole _ a -> identRange a
  TypeString _ a _ -> srcRange a
  TypeRow _ a -> wrappedRange a
  TypeRecord _ a -> wrappedRange a
  TypeForall _ a _ _ b -> widen (srcRange a) (typeRange b)
  TypeKinded _ a _ b -> widen (typeRange a) (kindRange b)
  TypeApp _ a b -> widen (typeRange a) (typeRange b)
  TypeOp _ a _ b -> widen (typeRange a) (typeRange b)
  TypeOpName _ a -> wrappedRange a
  TypeArr _ a _ b -> widen (typeRange a) (typeRange b)
  TypeArrName _ a -> wrappedRange a
  TypeConstrained _ a _ b -> widen (typeRange a) (typeRange b)
  TypeParens _ a -> wrappedRange a

typeVarBindingRange :: TypeVarBinding a -> SourceRange
typeVarBindingRange = \case
  TypeVarKinded a -> wrappedRange a
  TypeVarName a -> identRange a

exprRange :: Expr a -> SourceRange
exprRange = \case
  ExprHole _ a -> identRange a
  ExprSection _ a -> srcRange a
  ExprIdent _ a -> identRange a
  ExprConstructor _ a -> identRange a
  ExprBoolean _ a _ -> srcRange a
  ExprChar _ a _ -> srcRange a
  ExprString _ a _ -> srcRange a
  ExprNumber _ a _ -> srcRange a
  ExprArray _ a -> wrappedRange a
  ExprRecord _ a -> wrappedRange a
  ExprParens _ a -> wrappedRange a
  ExprTyped _ a _ b -> widen (exprRange a) (typeRange b)
  ExprInfix _ a _ b -> widen (exprRange a) (exprRange b)
  ExprOp _ a _ b -> widen (exprRange a) (exprRange b)
  ExprOpName _ a -> wrappedRange a
  ExprNegate _ a b -> widen (srcRange a) (exprRange b)
  ExprRecordAccessor _ (RecordAccessor a _ b) -> widen (exprRange a) (identRange $ sepLast b)
  ExprRecordUpdate _ a b -> widen (exprRange a) (wrappedRange b)
  ExprApp _ a b -> widen (exprRange a) (exprRange b)
  ExprLambda _ (Lambda a _ _ b) -> widen (srcRange a) (exprRange b)
  ExprIf _ (IfThenElse a _ _ _ _ b) -> widen (srcRange a) (exprRange b)
  ExprCase _ (CaseOf a _ b c)
    | [] <- c -> widen start $ srcRange b
    | otherwise -> widen start $ guardedRange $ snd $ last c
    where start = srcRange a
  ExprLet _ (LetIn a _ _ b) -> widen (srcRange a) (exprRange b)
  ExprWhere _ (Where a b c)
    | [] <- c -> widen start $ srcRange b
    | otherwise -> widen start $ letBindingRange $ last c
    where start = exprRange a
  ExprDo _ (DoBlock a b) -> widen (srcRange a) (doStatementRange $ last b)
  ExprAdo _ (AdoBlock a _ _ b) -> widen (srcRange a) (exprRange b)

letBindingRange :: LetBinding a -> SourceRange
letBindingRange = \case
  LetBindingSignature _ (Labeled a _ b) -> widen (identRange a) (typeRange b)
  LetBindingName _ a -> valueBindingFieldsRange a
  LetBindingPattern _ a _ b -> widen (binderRange a) (exprRange b)

doStatementRange :: DoStatement a -> SourceRange
doStatementRange = \case
  DoLet a bs -> widen (srcRange a) (letBindingRange $ last bs)
  DoDiscard a -> exprRange a
  DoBind a _ b -> widen (binderRange a) (exprRange b)

binderRange :: Binder a -> SourceRange
binderRange = \case
  BinderWildcard _ a -> srcRange a
  BinderVar _ a -> identRange a
  BinderNamed _ a _ b -> widen (identRange a) (binderRange b)
  BinderConstructor _ a bs
    | [] <- bs -> start
    | otherwise -> widen start $ binderRange $ last bs
    where start = identRange a
  BinderBoolean _ a _ -> srcRange a
  BinderChar _ a _ -> srcRange a
  BinderString _ a _ -> srcRange a
  BinderNumber _ a b _
    | Just a' <- a -> widen (srcRange a') end
    | otherwise -> end
    where end = srcRange b
  BinderArray _ a -> wrappedRange a
  BinderRecord _ a -> wrappedRange a
  BinderParens _ a -> wrappedRange a
  BinderTyped _ a _ b -> widen (binderRange a) (typeRange b)
  BinderOp _ a _ b -> widen (binderRange a) (binderRange b)
