{-# LANGUAGE MonoLocalBinds #-}
module Language.PureScript.CST.Utils where

import Prelude

import Control.Monad (when)
import Data.Coerce (coerce)
import Data.Foldable (for_)
import Data.Functor (($>))
import qualified Data.List.NonEmpty as NE
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Language.PureScript.CST.Errors
import Language.PureScript.CST.Monad
import Language.PureScript.CST.Positions
import Language.PureScript.CST.Traversals.Type
import Language.PureScript.CST.Types
import qualified Language.PureScript.Names as N

placeholder :: SourceToken
placeholder = SourceToken
  { tokAnn = TokenAnn (SourceRange (SourcePos 0 0) (SourcePos 0 0)) [] []
  , tokValue = TokLowerName [] "<placeholder>"
  }

unexpectedName :: SourceToken -> Name Ident
unexpectedName tok = Name tok (Ident "<unexpected>")

unexpectedQual :: SourceToken -> QualifiedName Ident
unexpectedQual tok = QualifiedName tok Nothing (Ident "<unexpected>")

unexpectedLabel :: SourceToken -> Label
unexpectedLabel tok = Label tok "<unexpected>"

unexpectedExpr :: Monoid a => [SourceToken] -> Expr a
unexpectedExpr toks = ExprIdent mempty (unexpectedQual (head toks))

unexpectedDecl :: Monoid a => [SourceToken] -> Declaration a
unexpectedDecl toks = DeclValue mempty (ValueBindingFields (unexpectedName (head toks)) [] (error "<unexpected"))

unexpectedBinder :: Monoid a => [SourceToken] -> Binder a
unexpectedBinder toks = BinderVar mempty (unexpectedName (head toks))

unexpectedLetBinding :: Monoid a => [SourceToken] -> LetBinding a
unexpectedLetBinding toks = LetBindingName mempty (ValueBindingFields (unexpectedName (head toks)) [] (error "<unexpected>"))

unexpectedInstBinding :: Monoid a => [SourceToken] -> InstanceBinding a
unexpectedInstBinding toks = InstanceBindingName mempty (ValueBindingFields (unexpectedName (head toks)) [] (error "<unexpected>"))

unexpectedRecordUpdate :: Monoid a => [SourceToken] -> RecordUpdate a
unexpectedRecordUpdate toks = RecordUpdateLeaf (unexpectedLabel (head toks)) (head toks) (unexpectedExpr toks)

unexpectedRecordLabeled :: [SourceToken] -> RecordLabeled a
unexpectedRecordLabeled toks = RecordPun (unexpectedName (head toks))

rangeToks :: TokenRange -> [SourceToken]
rangeToks (a, b) = [a, b]

unexpectedToks :: (a -> TokenRange) -> ([SourceToken] -> b) -> ParserErrorType -> (a -> Parser b)
unexpectedToks toRange toCst err old = do
  let toks = rangeToks $ toRange old
  addFailure toks err
  pure $ toCst toks

separated :: [(SourceToken, a)] -> Separated a
separated = go []
  where
  go accum ((_, a) : []) = Separated a accum
  go accum (x : xs) = go (x : accum) xs
  go _ [] = internalError "Separated should not be empty"

consSeparated :: a -> SourceToken -> Separated a -> Separated a
consSeparated x sep (Separated {..}) = Separated x ((sep, sepHead) : sepTail)

internalError :: String -> a
internalError = error . ("Internal parser error: " <>)

toModuleName :: SourceToken -> [Text] -> Parser (Maybe N.ModuleName)
toModuleName _ [] = pure Nothing
toModuleName tok ns = do
  when (not (all isValidModuleNamespace ns)) $ addFailure [tok] ErrModuleName
  pure . Just . N.ModuleName $ N.ProperName <$> ns

upperToModuleName :: SourceToken -> Parser (Name N.ModuleName)
upperToModuleName tok = case tokValue tok of
  TokUpperName q a -> do
    let ns = q <> [a]
    when (not (all isValidModuleNamespace ns)) $ addFailure [tok] ErrModuleName
    pure . Name tok . N.ModuleName $ N.ProperName <$> ns
  _ -> internalError $ "Invalid upper name: " <> show tok

toQualifiedName :: (Text -> a) -> SourceToken -> Parser (QualifiedName a)
toQualifiedName k tok = case tokValue tok of
  TokLowerName q a
    | not (Set.member a reservedNames) -> flip (QualifiedName tok) (k a) <$> toModuleName tok q
    | otherwise -> addFailure [tok] ErrKeywordVar $> QualifiedName tok Nothing (k "<unexpected>")
  TokUpperName q a  -> flip (QualifiedName tok) (k a) <$> toModuleName tok q
  TokSymbolName q a -> flip (QualifiedName tok) (k a) <$> toModuleName tok q
  TokOperator q a   -> flip (QualifiedName tok) (k a) <$> toModuleName tok q
  _                 -> internalError $ "Invalid qualified name: " <> show tok

toName :: (Text -> a) -> SourceToken -> Parser (Name a)
toName k tok = case tokValue tok of
  TokLowerName [] a
    | not (Set.member a reservedNames) -> pure $ Name tok (k a)
    | otherwise -> addFailure [tok] ErrKeywordVar $> Name tok (k "<unexpected>")
  TokUpperName [] a  -> pure $ Name tok (k a)
  TokSymbolName [] a -> pure $ Name tok (k a)
  TokOperator [] a   -> pure $ Name tok (k a)
  _                  -> internalError $ "Invalid name: " <> show tok

toLabel :: SourceToken -> Label
toLabel tok = case tokValue tok of
  TokLowerName [] a -> Label tok a
  TokString _ a     -> Label tok a
  TokRawString a    -> Label tok a
  TokForall ASCII   -> Label tok "forall"
  _                 -> internalError $ "Invalid label: " <> show tok

labelToIdent :: Label -> Parser (Name Ident)
labelToIdent (Label tok _) = toName Ident tok

toString :: SourceToken -> (SourceToken, Text)
toString tok = case tokValue tok of
  TokString _ a  -> (tok, a)
  TokRawString a -> (tok, a)
  _              -> internalError $ "Invalid string literal: " <> show tok

toChar :: SourceToken -> (SourceToken, Char)
toChar tok = case tokValue tok of
  TokChar _ a -> (tok, a)
  _           -> internalError $ "Invalid char literal: " <> show tok

toNumber :: SourceToken -> (SourceToken, Either Integer Double)
toNumber tok = case tokValue tok of
  TokInt _ a    -> (tok, Left a)
  TokNumber _ a -> (tok, Right a)
  _             -> internalError $ "Invalid number literal: " <> show tok

toInt :: SourceToken -> (SourceToken, Integer)
toInt tok = case tokValue tok of
  TokInt _ a    -> (tok, a)
  _             -> internalError $ "Invalid integer literal: " <> show tok

toBoolean :: SourceToken -> (SourceToken, Bool)
toBoolean tok = case tokValue tok of
  TokLowerName [] "true"  -> (tok, True)
  TokLowerName [] "false" -> (tok, False)
  _                       -> internalError $ "Invalid boolean literal: " <> show tok

toConstraint :: forall a. Monoid a => Type a -> Parser (Constraint a)
toConstraint = convertParens
  where
  convertParens :: Type a -> Parser (Constraint a)
  convertParens = \case
    TypeParens a (Wrapped b c d) -> do
      c' <- convertParens c
      pure $ ConstraintParens a (Wrapped b c' d)
    ty -> convert mempty [] ty

  convert :: a -> [Type a] -> Type a -> Parser (Constraint a)
  convert ann acc = \case
    TypeApp a lhs rhs -> convert (a <> ann) (rhs : acc) lhs
    TypeConstructor a name -> pure $ Constraint (a <> ann) (coerce name) acc
    ty -> do
      let (tok1, tok2) = typeRange ty
      addFailure [tok1, tok2] ErrTypeInConstraint
      pure $ Constraint mempty (QualifiedName tok1 Nothing (N.ProperName "<unexpected")) []

toBinderConstructor :: Monoid a => NE.NonEmpty (Binder a) -> Parser (Binder a)
toBinderConstructor = \case
  BinderConstructor a name [] NE.:| bs ->
    pure $ BinderConstructor a name bs
  a NE.:| [] -> pure a
  a NE.:| _ -> unexpectedToks binderRange (unexpectedBinder) ErrExprInBinder a

toLetBinding :: Monoid a => NE.NonEmpty (Binder a) -> Guarded a -> Parser (LetBinding a)
toLetBinding lhs guarded = case lhs of
  BinderVar ann ident NE.:| binders ->
    pure $ LetBindingName ann (ValueBindingFields ident binders guarded)
  BinderConstructor ann name [] NE.:| binders -> do
    mkLetBindingPattern $ BinderConstructor ann name binders
  binder NE.:| [] ->
    mkLetBindingPattern binder
  binders ->
    unexpectedToks binderRange unexpectedLetBinding ErrExprInDeclOrBinder (NE.head binders)
  where
  mkLetBindingPattern binder = case guarded of
    Unconditional tok wh ->
      pure $ LetBindingPattern mempty binder tok wh
    Guarded gs ->
      unexpectedToks guardedExprRange unexpectedLetBinding ErrGuardInLetBinder (NE.head gs)

toRecordFields
  :: Monoid a
  => Separated (Either (RecordLabeled (Expr a)) (RecordUpdate a))
  -> Parser (Either (Separated (RecordLabeled (Expr a))) (Separated (RecordUpdate a)))
toRecordFields = \case
  Separated (Left a) as ->
    Left . Separated a <$> traverse (traverse unLeft) as
  Separated (Right a) as ->
    Right . Separated a <$> traverse (traverse unRight) as
  where
  unLeft (Left tok) = pure tok
  unLeft (Right tok) =
    unexpectedToks recordUpdateRange unexpectedRecordLabeled ErrRecordUpdateInCtr tok

  unRight (Right tok) = pure tok
  unRight (Left (RecordPun (Name tok _))) = do
    addFailure [tok] ErrRecordPunInUpdate
    pure $ unexpectedRecordUpdate [tok]
  unRight (Left (RecordField _ tok _)) = do
    addFailure [tok] ErrRecordCtrInUpdate
    pure $ unexpectedRecordUpdate [tok]

checkFundeps :: ClassHead a -> Parser ()
checkFundeps (ClassHead _ _ _ _ Nothing) = pure ()
checkFundeps (ClassHead _ _ _ vars (Just (_, fundeps))) = do
  let
    k (TypeVarKinded (Wrapped _ (Labeled a _ _) _)) = getIdent $ nameValue a
    k (TypeVarName a) = getIdent $ nameValue a
    names = k <$> vars
    check a
      | getIdent (nameValue a) `elem` names = pure ()
      | otherwise = addFailure [nameTok a] ErrUnknownFundep
  for_ fundeps $ \case
    FundepDetermined _ bs -> for_ bs check
    FundepDetermines as _ bs -> do
      for_ as check
      for_ bs check

data TmpModuleDecl a
  = TmpImport (ImportDecl a)
  | TmpDecl (Declaration a)
  | TmpChain SourceToken (Declaration a)

toModuleDecls :: Monoid a => [TmpModuleDecl a] -> Parser ([ImportDecl a], [Declaration a])
toModuleDecls = goImport []
  where
  goImport acc (TmpImport x : xs) = goImport (x : acc) xs
  goImport acc xs = (reverse acc,) <$> goDecl [] xs

  goDecl acc [] = pure $ reverse acc
  goDecl acc (TmpDecl x : xs) = goDecl (x : acc) xs
  goDecl (DeclInstanceChain a (Separated h t) : acc) (TmpChain tok (DeclInstanceChain a' (Separated h' t')) : xs) = do
    let getName = instName . instHead
    when (getName h == getName h') $ addFailure [nameTok $ getName h'] ErrInstanceNameMismatch
    goDecl (DeclInstanceChain (a <> a') (Separated h (t <> ((tok, h') : t'))) : acc) xs
  goDecl _ (TmpChain tok _ : _) = addFailure [tok] ErrElseInDecl $> [unexpectedDecl [tok]]
  goDecl _ (TmpImport imp : _) = unexpectedToks importDeclRange (pure . unexpectedDecl) ErrImportInDecl imp

checkNoWildcards :: Type a -> Parser ()
checkNoWildcards ty = do
  let
    k = \case
      TypeWildcard _ a -> [addFailure [a] ErrWildcardInType]
      TypeHole _ a -> [addFailure [nameTok a] ErrHoleInType]
      _ -> []
  sequence_ $ everythingOnTypes (<>) k ty

revert :: Parser a -> SourceToken -> Parser a
revert p lk = pushBack lk *> p

reservedNames :: Set Text
reservedNames = Set.fromList
  [ "ado"
  , "case"
  , "class"
  , "data"
  , "derive"
  , "do"
  , "else"
  , "false"
  , "forall"
  , "foreign"
  , "import"
  , "if"
  , "in"
  , "infix"
  , "infixl"
  , "infixr"
  , "instance"
  , "let"
  , "module"
  , "newtype"
  , "of"
  , "true"
  , "type"
  , "where"
  ]

isValidModuleNamespace :: Text -> Bool
isValidModuleNamespace = Text.null . snd . Text.span (\c -> c /= '_' && c /= '\'')
