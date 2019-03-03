{-# LANGUAGE TypeApplications #-}
module Language.PureScript.CST.Convert where

import Prelude

import Data.Coerce (coerce)
import Data.Functor (($>))
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Language.PureScript.AST as AST
import qualified Language.PureScript.AST.SourcePos as Pos
import qualified Language.PureScript.Comments as C
import qualified Language.PureScript.Environment as E
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

qualified :: forall a. (Text -> a) -> Ident -> N.Qualified a
qualified k (Ident _ qual name) = N.Qualified modName (k name)
  where
  modName = case qual of
    [] -> Nothing
    _  -> Just $ N.ModuleName $ N.ProperName <$> qual

properName :: forall a. Ident -> N.Qualified (N.ProperName a)
properName = qualified N.ProperName

opName :: forall a. Ident -> N.Qualified (N.OpName a)
opName = qualified N.OpName

properKind :: Ident -> N.Qualified (N.ProperName 'N.KindName)
properKind = properName @'N.KindName

properType :: Ident -> N.Qualified (N.ProperName 'N.TypeName)
properType = properName @'N.TypeName

typeOp :: Ident -> N.Qualified (N.OpName 'N.TypeOpName)
typeOp = opName @'N.TypeOpName

convertKind :: String -> Kind a -> K.SourceKind
convertKind fileName = go
  where
  go = \case
    KindName _ ident ->
      K.NamedKind (sourceIdent fileName ident) $ properKind ident
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
      T.TypeApp ann (E.tyRecord $> annRec) $ goRow row b
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
    TypeOp _ a op b -> do
      let
        a' = go a
        b' = go b
        op' = T.TypeOp (sourceIdent fileName op) (typeOp op)
        ann = Pos.widenSourceAnn (T.getAnnForType a') (T.getAnnForType b')
      T.BinaryNoParensType ann a' b' op'
    TypeOpName _ (Wrapped a op b) ->
      T.TypeOp (sourceAnnCommented fileName a b) (typeOp op)
    TypeArr _ a arr b -> do
      let
        a' = go a
        b' = go b
        arr' = E.tyFunction $> sourceAnnCommented fileName arr arr
        ann = Pos.widenSourceAnn (T.getAnnForType a') (T.getAnnForType b')
      T.TypeApp ann (T.TypeApp ann arr' a') b'
    TypeArrName _ (Wrapped a _ b) ->
      E.tyFunction $> sourceAnnCommented fileName a b
    TypeConstrained _ a _ b -> do
      let
        a' = go a
        b' = go b
        ann = Pos.widenSourceAnn (T.getAnnForType a') (T.getAnnForType b')
        k acc (T.TypeApp _ x y) = k (y : acc) x
        k acc (T.TypeConstructor ann' name) =
          T.Constraint (T.getAnnForType a' $> snd ann') (coerce name) (reverse acc) Nothing
        k _ _ = error "Invalid constraint"
      T.ConstrainedType ann (k [] a') b'
    TypeParens _ (Wrapped a ty b) ->
      T.ParensInType (sourceAnnCommented fileName a b) $ go ty
