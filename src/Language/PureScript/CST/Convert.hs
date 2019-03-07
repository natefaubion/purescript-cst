{-# LANGUAGE TypeApplications #-}
module Language.PureScript.CST.Convert
  ( convertKind
  , convertType
  , convertExpr
  , convertBinder
  , convertDeclaration
  , convertModule
  ) where

import Prelude

import Data.Bifunctor (bimap)
import Data.Coerce (coerce)
import Data.Foldable (foldl', toList)
import Data.Functor (($>))
import Data.Maybe (isJust, fromJust, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Language.PureScript.AST as AST
import qualified Language.PureScript.AST.SourcePos as Pos
import qualified Language.PureScript.Comments as C
import qualified Language.PureScript.Environment as Env
import qualified Language.PureScript.Kinds as K
import qualified Language.PureScript.Label as L
import qualified Language.PureScript.Names as N
import Language.PureScript.PSString (mkString)
import qualified Language.PureScript.Types as T
import Language.PureScript.CST.Positions
import Language.PureScript.CST.Types

comment :: Comment a -> Maybe C.Comment
comment = \case
  Comment t
    | Text.isPrefixOf "{-" t -> Just $ C.BlockComment $ Text.drop 2 $ Text.dropEnd 2 t
    | Text.isPrefixOf "--" t -> Just $ C.LineComment $ Text.drop 2 t
  _ -> Nothing

comments :: [Comment a] -> [C.Comment]
comments = mapMaybe comment

sourcePos :: SourcePos -> Pos.SourcePos
sourcePos (SourcePos line col) = Pos.SourcePos line col

sourceSpan :: String -> SourceRange -> Pos.SourceSpan
sourceSpan name (SourceRange start end) = Pos.SourceSpan name (sourcePos start) (sourcePos end)

widenLeft :: TokenAnn -> Pos.SourceAnn -> Pos.SourceAnn
widenLeft ann (sp, _) =
  ( Pos.widenSourceSpan (sourceSpan (Pos.spanName sp) $ tokRange ann) sp
  , comments $ tokLeadingComments ann
  )

sourceAnnCommented :: String -> SourceToken -> SourceToken -> Pos.SourceAnn
sourceAnnCommented fileName (ann1, _) (ann2, _) =
  ( Pos.SourceSpan fileName (sourcePos $ srcStart $ tokRange ann1) (sourcePos $ srcEnd $ tokRange ann2)
  , comments $ tokLeadingComments ann1
  )

sourceAnn :: String -> SourceToken -> SourceToken -> Pos.SourceAnn
sourceAnn fileName (ann1, _) (ann2, _) =
  ( Pos.SourceSpan fileName (sourcePos $ srcStart $ tokRange ann1) (sourcePos $ srcEnd $ tokRange ann2)
  , []
  )

sourceIdent :: String -> Ident -> Pos.SourceAnn
sourceIdent fileName a = sourceAnnCommented fileName (identTok a) (identTok a)

moduleName :: Token -> Maybe N.ModuleName
moduleName = \case
  TokLowerName as _ -> go as
  TokUpperName as _ -> go as
  TokSymbol as _ -> go as
  _ -> Nothing
  where
  go [] = Nothing
  go ns = Just $ N.ModuleName $ N.ProperName <$> ns

moduleName' :: Ident -> N.ModuleName
moduleName' i = N.ModuleName $ N.ProperName <$> identQual i <> [identName i]

qualified :: forall a. (Text -> a) -> Ident -> N.Qualified a
qualified k i = N.Qualified (moduleName . snd $ identTok i) . k $ identName i

properName :: forall a. Ident -> N.Qualified (N.ProperName a)
properName = qualified N.ProperName

opName :: forall a. Ident -> N.Qualified (N.OpName a)
opName = qualified N.OpName

properKind :: Ident -> N.Qualified (N.ProperName 'N.KindName)
properKind = properName @'N.KindName

properType :: Ident -> N.Qualified (N.ProperName 'N.TypeName)
properType = properName @'N.TypeName

properCtr :: Ident -> N.Qualified (N.ProperName 'N.ConstructorName)
properCtr = properName @'N.ConstructorName

properClass :: Ident -> N.Qualified (N.ProperName 'N.ClassName)
properClass = properName @'N.ClassName

typeOp :: Ident -> N.Qualified (N.OpName 'N.TypeOpName)
typeOp = opName @'N.TypeOpName

valueOp :: Ident -> N.Qualified (N.OpName 'N.ValueOpName)
valueOp = opName @'N.ValueOpName

ident :: Ident -> N.Qualified N.Ident
ident = qualified N.Ident

convertKind :: String -> Kind a -> K.SourceKind
convertKind fileName = go
  where
  go = \case
    KindName _ a ->
      K.NamedKind (sourceIdent fileName a) $ properKind a
    KindArr _ a _ b -> do
      let
        lhs = go a
        rhs = go b
        ann = Pos.widenSourceAnn (K.getAnnForKind lhs) (K.getAnnForKind rhs)
      K.FunKind ann lhs rhs
    KindRow _ tok a -> do
      let
        kind = go a
        ann = widenLeft (fst tok) $ K.getAnnForKind kind
      K.Row ann kind
    KindParens _ (Wrapped _ a _) ->
      go a

convertType :: String -> Type a -> T.SourceType
convertType fileName = go
  where
  goRow (Row labels tl) b = do
    let
      rowTail = case tl of
        Just (_, ty) -> go ty
        Nothing -> T.REmpty $ sourceAnnCommented fileName b b
      rowCons (Labeled a _ ty) c = do
        let ann = sourceAnnCommented fileName (identTok a) (snd $ typeRange ty)
        T.RCons ann (L.Label . mkString $ identName a) (go ty) c
    case labels of
      Just (Separated h t) ->
        rowCons h $ foldr (rowCons . snd) rowTail t
      Nothing ->
        rowTail

  go = \case
    TypeVar _ a ->
      T.TypeVar (sourceIdent fileName a) (identName a)
    TypeConstructor _ a ->
      T.TypeConstructor (sourceIdent fileName a) $ properType a
    TypeWildcard _ a ->
      T.TypeWildcard (sourceAnnCommented fileName a a) Nothing
    TypeHole _ a ->
      T.TypeWildcard (sourceIdent fileName a) . Just $ identName a
    TypeString _ a b ->
      T.TypeLevelString (sourceAnnCommented fileName a a) $ mkString b
    TypeRow _ (Wrapped _ row b) ->
      goRow row b
    TypeRecord _ (Wrapped a row b) -> do
      let
        ann = sourceAnnCommented fileName a b
        annRec = sourceAnn fileName a a
      T.TypeApp ann (Env.tyRecord $> annRec) $ goRow row b
    TypeForall _ _ bindings _ ty -> do
      let
        mkForAll a t = T.ForAll (sourceIdent fileName a) (identName a) t Nothing
        -- TODO: fix forall in the compiler
        k (TypeVarKinded (Wrapped _ (Labeled a _ _) _)) = mkForAll a
        k (TypeVarName a) = mkForAll a
      foldr k (go ty) bindings
    TypeKinded _ ty _ kd -> do
      let
        ty' = go ty
        kd' = convertKind fileName kd
        ann = Pos.widenSourceAnn (T.getAnnForType ty') (K.getAnnForKind kd')
      T.KindedType ann ty' kd'
    TypeApp _ a b -> do
      let
        a' = go a
        b' = go b
        ann = Pos.widenSourceAnn (T.getAnnForType a') (T.getAnnForType b')
      T.TypeApp ann a' b'
    ty@(TypeOp _ _ _ _) -> do
      let
        reassoc op b' a = do
          let
            a'  = go a
            op' = T.TypeOp (sourceIdent fileName op) $ typeOp op
            ann = Pos.widenSourceAnn (T.getAnnForType a') (T.getAnnForType b')
          T.BinaryNoParensType ann op' (go a) b'
        loop k = \case
          TypeOp _ a op b -> loop (reassoc op (k b)) a
          expr' -> k expr'
      loop go ty
    TypeOpName _ (Wrapped a op b) ->
      T.TypeOp (sourceAnnCommented fileName a b) (typeOp op)
    TypeArr _ a arr b -> do
      let
        a' = go a
        b' = go b
        arr' = Env.tyFunction $> sourceAnnCommented fileName arr arr
        ann = Pos.widenSourceAnn (T.getAnnForType a') (T.getAnnForType b')
      T.TypeApp ann (T.TypeApp ann arr' a') b'
    TypeArrName _ (Wrapped a _ b) ->
      Env.tyFunction $> sourceAnnCommented fileName a b
    TypeConstrained _ a _ b -> do
      let
        a' = go a
        b' = go b
        ann = Pos.widenSourceAnn (T.getAnnForType a') (T.getAnnForType b')
      T.ConstrainedType ann (typeToConstraint a') b'
    TypeParens _ (Wrapped a ty b) ->
      T.ParensInType (sourceAnnCommented fileName a b) $ go ty

typeToConstraint :: T.SourceType -> T.SourceConstraint
typeToConstraint ty = go [] ty
  where
  go acc (T.ParensInType _ x) = go acc x
  go acc (T.TypeApp _ x y) = go (y : acc) x
  go acc (T.TypeConstructor ann' name) =
    T.Constraint (T.getAnnForType ty $> snd ann') (coerce name) acc Nothing
  go _ _ = error $ "Invalid constraint: \n" <> show ty

convertGuarded :: String -> Guarded a -> [AST.GuardedExpr]
convertGuarded fileName = \case
  Unconditional _ x -> [AST.GuardedExpr [] (go x)]
  Guarded gs -> (\(GuardedExpr _ ps _ x) -> AST.GuardedExpr (p <$> toList ps) (go x)) <$> gs
  where
  go = convertExpr fileName
  p (PatternGuard Nothing x) = AST.ConditionGuard (go x)
  p (PatternGuard (Just (b, _)) x) = AST.PatternGuard (convertBinder fileName b) (go x)

convertExpr :: forall a. String -> Expr a -> AST.Expr
convertExpr fileName = go
  where
  positioned =
    uncurry AST.PositionedValue

  goLetBinding = \case
    LetBindingSignature _ lbl ->
      convertSignature fileName lbl
    binding@(LetBindingName _ fields) -> do
      let ann = uncurry (sourceAnnCommented fileName) $ letBindingRange binding
      convertValueBindingFields fileName ann fields
    binding@(LetBindingPattern _ a _ b) -> do
      let ann = uncurry (sourceAnnCommented fileName) $ letBindingRange binding
      AST.BoundValueDeclaration ann (convertBinder fileName a) (go b)

  goDoStatement = \case
    stmt@(DoLet _ as) -> do
      let ann = uncurry (sourceAnnCommented fileName) $ doStatementRange stmt
      uncurry AST.PositionedDoNotationElement ann . AST.DoNotationLet $ goLetBinding <$> as
    stmt@(DoDiscard a) -> do
      let ann = uncurry (sourceAnn fileName) $ doStatementRange stmt
      uncurry AST.PositionedDoNotationElement ann . AST.DoNotationValue $ go a
    stmt@(DoBind a _ b) -> do
      let
        ann = uncurry (sourceAnn fileName) $ doStatementRange stmt
        a' = convertBinder fileName a
        b' = go b
      uncurry AST.PositionedDoNotationElement ann $ AST.DoNotationBind a' b'

  go = \case
    ExprHole _ a ->
      positioned (sourceIdent fileName a) . AST.Hole $ identName a
    ExprSection _ a ->
      positioned (sourceAnnCommented fileName a a) AST.AnonymousArgument
    ExprIdent _ a -> do
      let ann = sourceIdent fileName a
      positioned ann . AST.Var (fst ann) $ ident a
    ExprConstructor _ a -> do
      let ann = sourceIdent fileName a
      positioned ann . AST.Constructor (fst ann) $ properCtr a
    ExprBoolean _ a b -> do
      let ann = sourceAnnCommented fileName a a
      positioned ann . AST.Literal (fst ann) $ AST.BooleanLiteral b
    ExprChar _ a b -> do
      let ann = sourceAnnCommented fileName a a
      positioned ann . AST.Literal (fst ann) $ AST.CharLiteral b
    ExprString _ a b -> do
      let ann = sourceAnnCommented fileName a a
      positioned ann . AST.Literal (fst ann) . AST.StringLiteral $ mkString b
    ExprNumber _ a b -> do
      let ann = sourceAnnCommented fileName a a
      positioned ann . AST.Literal (fst ann) $ AST.NumericLiteral b
    ExprArray _ (Wrapped a bs c) -> do
      let
        ann = sourceAnnCommented fileName a c
        vals = case bs of
          Just (Separated x xs) -> go x : (go . snd <$> xs)
          Nothing -> []
      positioned ann . AST.Literal (fst ann) $ AST.ArrayLiteral vals
    ExprRecord z (Wrapped a bs c) -> do
      let
        ann = sourceAnnCommented fileName a c
        lbl = \case
          RecordPun f -> (mkString $ identName f, go $ ExprIdent z f)
          RecordField f _ v -> (mkString $ identName f, go v)
        vals = case bs of
          Just (Separated x xs) -> lbl x : (lbl . snd <$> xs)
          Nothing -> []
      positioned ann . AST.Literal (fst ann) $ AST.ObjectLiteral vals
    ExprParens _ (Wrapped a b c) ->
      positioned (sourceAnnCommented fileName a c) . AST.Parens $ go b
    expr@(ExprTyped _ a _ b) -> do
      let
        a' = go a
        b' = convertType fileName b
        ann = (sourceSpan fileName . toSourceRange $ exprRange expr, [])
      positioned ann $ AST.TypedValue True a' b'
    expr@(ExprInfix _ a (Wrapped _ b _) c) -> do
      let ann = (sourceSpan fileName . toSourceRange $ exprRange expr, [])
      positioned ann $ AST.BinaryNoParens (go b) (go a) (go c)
    expr@(ExprOp _ _ _ _) -> do
      let
        ann = uncurry (sourceAnn fileName) $ exprRange expr
        reassoc op b a = do
          let op' = AST.Op (sourceSpan fileName . toSourceRange $ identRange op) $ valueOp op
          AST.BinaryNoParens op' (go a) b
        loop k = \case
          ExprOp _ a op b -> loop (reassoc op (k b)) a
          expr' -> k expr'
      positioned ann $ loop go expr
    ExprOpName _ (Wrapped a op b) -> do
      let op' = AST.Op (sourceSpan fileName . toSourceRange $ identRange op) $ valueOp op
      positioned (sourceAnnCommented fileName a b) $ AST.Parens op'
    expr@(ExprNegate _ _ b) -> do
      let ann = uncurry (sourceAnnCommented fileName) $ exprRange expr
      positioned ann . AST.UnaryMinus (fst ann) $ go b
    expr@(ExprRecordAccessor _ (RecordAccessor a _ (Separated h t))) -> do
      let
        ann = uncurry (sourceAnnCommented fileName) $ exprRange expr
        field x f = AST.Accessor (mkString $ identName f) x
      positioned ann $ foldl' (\x (_, f) -> field x f) (field (go a) h) t
    expr@(ExprRecordUpdate _ a b) -> do
      let
        ann = uncurry (sourceAnnCommented fileName) $ exprRange expr
        k (RecordUpdateLeaf f _ x) = (mkString $ identName f, AST.Leaf $ go x)
        k (RecordUpdateBranch f xs) = (mkString $ identName f, AST.Branch $ toTree xs)
        toTree (Wrapped _ xs _) = AST.PathTree . AST.AssocList . map k $ toList xs
      positioned ann . AST.ObjectUpdateNested (go a) $ toTree b
    expr@(ExprApp _ a b) -> do
      let ann = uncurry (sourceAnn fileName) $ exprRange expr
      positioned ann $ AST.App (go a) (go b)
    expr@(ExprLambda _ (Lambda _ as _ b)) -> do
      let ann = uncurry (sourceAnnCommented fileName) $ exprRange expr
      positioned ann
        . AST.Abs (convertBinder fileName (head as))
        . foldr (AST.Abs . convertBinder fileName) (go b)
        $ tail as
    expr@(ExprIf _ (IfThenElse _ a _ b _ c)) -> do
      let ann = uncurry (sourceAnnCommented fileName) $ exprRange expr
      positioned ann $ AST.IfThenElse (go a) (go b) (go c)
    expr@(ExprCase _ (CaseOf _ as _ bs)) -> do
      let
        ann = uncurry (sourceAnnCommented fileName) $ exprRange expr
        as' = go <$> toList as
        bs' = uncurry AST.CaseAlternative . bimap (map (convertBinder fileName) . toList) (convertGuarded fileName) <$> bs
      positioned ann $ AST.Case as' bs'
    expr@(ExprLet _ (LetIn _ as _ b)) -> do
      let ann = uncurry (sourceAnnCommented fileName) $ exprRange expr
      positioned ann . AST.Let AST.FromLet (goLetBinding <$> as) $ go b
    expr@(ExprWhere _ (Where a _ bs)) -> do
      let ann = uncurry (sourceAnnCommented fileName) $ exprRange expr
      positioned ann . AST.Let AST.FromWhere (goLetBinding <$> bs) $ go a
    expr@(ExprDo _ (DoBlock kw stmts)) -> do
      let ann = uncurry (sourceAnnCommented fileName) $ exprRange expr
      positioned ann . AST.Do (moduleName $ snd kw) $ goDoStatement <$> stmts
    expr@(ExprAdo _ (AdoBlock kw stms _ a)) -> do
      let ann = uncurry (sourceAnnCommented fileName) $ exprRange expr
      positioned ann . AST.Ado (moduleName $ snd kw) (goDoStatement <$> stms) $ go a

convertBinder :: String -> Binder a -> AST.Binder
convertBinder fileName = go
  where
  positioned =
    uncurry AST.PositionedBinder

  go = \case
    BinderWildcard _ a ->
      positioned (sourceAnnCommented fileName a a) AST.NullBinder
    BinderVar _ a -> do
      let ann = sourceIdent fileName a
      positioned ann . AST.VarBinder (fst ann) . N.Ident $ identName a
    binder@(BinderNamed _ a _ b) -> do
      let ann = uncurry (sourceAnnCommented fileName) $ binderRange binder
      positioned ann . AST.NamedBinder (fst ann) (N.Ident $ identName a) $ go b
    binder@(BinderConstructor _ a bs) -> do
      let ann = uncurry (sourceAnnCommented fileName) $ binderRange binder
      positioned ann . AST.ConstructorBinder (fst ann) (properCtr a) $ go <$> bs
    BinderBoolean _ a b -> do
      let ann = sourceAnnCommented fileName a a
      positioned ann . AST.LiteralBinder (fst ann) $ AST.BooleanLiteral b
    BinderChar _ a b -> do
      let ann = sourceAnnCommented fileName a a
      positioned ann . AST.LiteralBinder (fst ann) $ AST.CharLiteral b
    BinderString _ a b -> do
      let ann = sourceAnnCommented fileName a a
      positioned ann . AST.LiteralBinder (fst ann) . AST.StringLiteral $ mkString b
    BinderNumber _ n a b -> do
      let
        ann = sourceAnnCommented fileName a a
        b'
          | isJust n = bimap negate negate b
          | otherwise = b
      positioned ann . AST.LiteralBinder (fst ann) $ AST.NumericLiteral b'
    BinderArray _ (Wrapped a bs c) -> do
      let
        ann = sourceAnnCommented fileName a c
        vals = case bs of
          Just (Separated x xs) -> go x : (go . snd <$> xs)
          Nothing -> []
      positioned ann . AST.LiteralBinder (fst ann) $ AST.ArrayLiteral vals
    BinderRecord z (Wrapped a bs c) -> do
      let
        ann = sourceAnnCommented fileName a c
        lbl = \case
          RecordPun f -> (mkString $ identName f, go $ BinderVar z f)
          RecordField f _ v -> (mkString $ identName f, go v)
        vals = case bs of
          Just (Separated x xs) -> lbl x : (lbl . snd <$> xs)
          Nothing -> []
      positioned ann . AST.LiteralBinder (fst ann) $ AST.ObjectLiteral vals
    BinderParens _ (Wrapped a b c) ->
      positioned (sourceAnnCommented fileName a c) . AST.ParensInBinder $ go b
    binder@(BinderTyped _ a _ b) -> do
      let
        a' = go a
        b' = convertType fileName b
        ann = (sourceSpan fileName . toSourceRange $ binderRange binder, [])
      positioned ann $ AST.TypedBinder b' a'
    binder@(BinderOp _ _ _ _) -> do
      let
        ann = uncurry (sourceAnn fileName) $ binderRange binder
        reassoc op b a = do
          let op' = AST.OpBinder (sourceSpan fileName . toSourceRange $ identRange op) $ valueOp op
          AST.BinaryNoParensBinder op' (go a) b
        loop k = \case
          BinderOp _ a op b -> loop (reassoc op (k b)) a
          binder' -> k binder'
      positioned ann $ loop go binder

convertDeclaration :: String -> Declaration a -> [AST.Declaration]
convertDeclaration fileName decl = case decl of
  DeclData _ (DataHead _ a vars) bd -> do
    let
      ctr (DataCtor _ x ys) = (N.ProperName $ identName x, convertType fileName <$> ys)
      ctrs = case bd of
        Nothing -> []
        Just (_, cs) -> ctr <$> toList cs
    pure $ AST.DataDeclaration ann Env.Data (N.ProperName $ identName a) (goTypeVar <$> vars) ctrs
  DeclType _ (DataHead _ a vars) _ bd ->
    pure $ AST.TypeSynonymDeclaration ann
      (N.ProperName $ identName a)
      (goTypeVar <$> vars)
      (convertType fileName bd)
  DeclNewtype _ (DataHead _ a vars) _ x ys -> do
    let ctrs = [(N.ProperName $ identName x, [convertType fileName ys])]
    pure $ AST.DataDeclaration ann Env.Newtype (N.ProperName $ identName a) (goTypeVar <$> vars) ctrs
  DeclClass _ (ClassHead _ sup name vars fdeps) bd -> do
    let
      goTyVar (TypeVarKinded (Wrapped _ (Labeled a _ _) _)) = identName a
      goTyVar (TypeVarName a) = identName a
      vars' = zip (toList $ goTyVar <$> vars) [0..]
      goName = fromJust . flip lookup vars' . identName
      goFundep (ClassFundep as _ bs) = Env.FunctionalDependency (goName <$> as) (goName <$> bs)
      goSig (Labeled n _ ty) = do
        let
          ty' = convertType fileName ty
          ann' = widenLeft (fst $ identTok n) $ T.getAnnForType ty'
        AST.TypeDeclaration $ AST.TypeDeclarationData ann' (N.Ident $ identName n) ty'
    pure $ AST.TypeClassDeclaration ann
      (N.ProperName $ identName name)
      (goTypeVar <$> vars)
      (typeToConstraint . convertType fileName <$> maybe [] (toList . fst) sup)
      (goFundep <$> maybe [] (toList . snd) fdeps)
      (goSig <$> maybe [] snd bd )
  DeclInstanceChain _ insts -> do
    let
      instName (Instance (InstanceHead _ a _ _ _ _) _) = N.Ident $ identName a
      chainId = instName <$> toList insts
      goInst ix inst@(Instance (InstanceHead _ name _ ctrs cls args) bd) = do
        let ann' = uncurry (sourceAnnCommented fileName) $ instanceRange inst
        AST.TypeInstanceDeclaration ann' chainId ix
          (N.Ident $ identName name)
          (typeToConstraint . convertType fileName <$> maybe [] (toList . fst) ctrs)
          (properClass cls)
          (convertType fileName <$> args)
          (AST.ExplicitInstance $ goInstanceBinding <$> maybe [] snd bd)
    uncurry goInst <$> zip [0..] (toList insts)
  DeclDerive _ _ new (InstanceHead _ name _ ctrs cls args) -> do
    let
      name' = N.Ident $ identName name
      instTy
        | isJust new = AST.NewtypeInstance
        | otherwise = AST.DerivedInstance
    pure $ AST.TypeInstanceDeclaration ann [name'] 0 name'
      (typeToConstraint . convertType fileName <$> maybe [] (toList . fst) ctrs)
      (properClass cls)
      (convertType fileName <$> args)
      instTy
  DeclSignature _ lbl ->
    pure $ convertSignature fileName lbl
  DeclValue _ fields ->
    pure $ convertValueBindingFields fileName ann fields
  DeclFixity _ (FixityFields kw (_, prec) mbTy name _ op) -> do
    let
      assoc =  case snd kw of
        TokLowerName [] "infixr" -> AST.Infixr
        TokLowerName [] "infixl" -> AST.Infixl
        _ -> AST.Infix
      fixity = AST.Fixity assoc prec
    pure $ AST.FixityDeclaration ann $ case mbTy of
      Nothing -> do
        let
          tok = snd $ identTok name
          name' = N.Qualified (moduleName tok) $ case tok of
            TokLowerName _ _ -> Left . N.Ident $ identName name
            _ -> Right $ N.ProperName $ identName name
        Left . AST.ValueFixity fixity name' . N.OpName $ identName op
      Just _ ->
        Right . AST.TypeFixity fixity (properType name) . N.OpName $ identName op
  DeclForeign _ _ _ frn ->
    pure $ case frn of
      ForeignValue (Labeled a _ b) ->
        AST.ExternDeclaration ann (N.Ident $ identName a) $ convertType fileName b
      ForeignData _ (Labeled a _ b) ->
        AST.ExternDataDeclaration ann (N.ProperName $ identName a) $ convertKind fileName b
      ForeignKind _ a ->
        AST.ExternKindDeclaration ann (N.ProperName $ identName a)
  where
  ann =
    uncurry (sourceAnnCommented fileName) $ declRange decl

  goTypeVar = \case
    TypeVarKinded (Wrapped _ (Labeled x _ y) _) -> (identName x, Just $ convertKind fileName y)
    TypeVarName x -> (identName x, Nothing)

  goInstanceBinding = \case
    InstanceBindingSignature _ lbl ->
      convertSignature fileName lbl
    binding@(InstanceBindingName _ fields) -> do
      let ann' = uncurry (sourceAnnCommented fileName) $ instanceBindingRange binding
      convertValueBindingFields fileName ann' fields

convertSignature :: String -> Labeled (Type a) -> AST.Declaration
convertSignature fileName (Labeled a _ b) = do
  let
    b' = convertType fileName b
    ann = widenLeft (fst $ identTok a) $ T.getAnnForType b'
  AST.TypeDeclaration $ AST.TypeDeclarationData ann (N.Ident $ identName a) b'

convertValueBindingFields :: String -> Pos.SourceAnn -> ValueBindingFields a -> AST.Declaration
convertValueBindingFields fileName ann (ValueBindingFields a bs c) = do
  let
    bs' = convertBinder fileName <$> bs
    cs' = convertGuarded fileName c
  AST.ValueDeclaration $ AST.ValueDeclarationData ann (N.Ident $ identName a) Env.Public bs' cs'

convertImportDecl :: String -> ImportDecl a -> AST.Declaration
convertImportDecl fileName decl@(ImportDecl _ _ modName mbNames mbQual) = do
  let
    ann = uncurry (sourceAnnCommented fileName) $ importDeclRange decl
    importTy = case mbNames of
      Nothing -> AST.Implicit
      Just (hiding, (Wrapped _ imps _)) -> do
        let imps' = convertImport fileName <$> toList imps
        if isJust hiding
          then AST.Hiding imps'
          else AST.Explicit imps'
  AST.ImportDeclaration ann (moduleName' modName) importTy (moduleName' . snd <$> mbQual)

convertImport :: String -> Import a -> AST.DeclarationRef
convertImport fileName imp = case imp of
  ImportValue _ a ->
    AST.ValueRef ann . N.Ident $ identName a
  ImportOp _ (Wrapped _ a _ ) ->
    AST.ValueOpRef ann . N.OpName $ identName a
  ImportType _ a mb -> do
    let
      ctrs = case mb of
        Nothing -> Just []
        Just (Wrapped _ Nothing _) -> Just []
        Just (Wrapped _ (Just (DataAll _ _)) _) -> Nothing
        Just (Wrapped _ (Just (DataEnumerated _ idents)) _) ->
          Just . map (N.ProperName . identName) $ toList idents
    AST.TypeRef ann (N.ProperName $ identName a) ctrs
  ImportTypeOp _ _ (Wrapped _ a _) ->
    AST.TypeOpRef ann . N.OpName $ identName a
  ImportClass _ _ a ->
    AST.TypeClassRef ann . N.ProperName $ identName a
  ImportKind _ _ a ->
    AST.KindRef ann . N.ProperName $ identName a
  where
  ann = sourceSpan fileName . toSourceRange $ importRange imp

convertExport :: String -> Export a -> AST.DeclarationRef
convertExport fileName export = case export of
  ExportValue _ a ->
    AST.ValueRef ann . N.Ident $ identName a
  ExportOp _ (Wrapped _ a _ ) ->
    AST.ValueOpRef ann . N.OpName $ identName a
  ExportType _ a mb -> do
    let
      ctrs = case mb of
        Nothing -> Just []
        Just (Wrapped _ Nothing _) -> Just []
        Just (Wrapped _ (Just (DataAll _ _)) _) -> Nothing
        Just (Wrapped _ (Just (DataEnumerated _ idents)) _) ->
          Just . map (N.ProperName . identName) $ toList idents
    AST.TypeRef ann (N.ProperName $ identName a) ctrs
  ExportTypeOp _ _ (Wrapped _ a _) ->
    AST.TypeOpRef ann . N.OpName $ identName a
  ExportClass _ _ a ->
    AST.TypeClassRef ann . N.ProperName $ identName a
  ExportKind _ _ a ->
    AST.KindRef ann . N.ProperName $ identName a
  ExportModule _ _ a ->
    AST.ModuleRef ann (moduleName' a)
  where
  ann = sourceSpan fileName . toSourceRange $ exportRange export

convertModule :: String -> Module a -> AST.Module
convertModule fileName module'@(Module _ _ modName exps _ imps decls _) = do
  let
    ann = uncurry (sourceAnnCommented fileName) $ moduleRange module'
    imps' = convertImportDecl fileName <$> imps
    decls' = convertDeclaration fileName =<< decls
    exps' = map (convertExport fileName) . toList . wrpValue <$> exps
  uncurry AST.Module ann (moduleName' modName) (imps' <> decls') exps'
