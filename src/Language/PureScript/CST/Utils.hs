module Language.PureScript.CST.Utils where

import Prelude

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Language.PureScript.CST.Types

placeholder :: SourceToken
placeholder =
  ( TokenAnn (SourceRange (SourcePos 0 0) (SourcePos 0 0)) [] []
  , TokLowerName [] "<placeholder>"
  )

separated :: [(SourceToken, a)] -> Separated a
separated = go []
  where
  go accum ((_, a) : []) = Separated a accum
  go accum (x : xs) = go (x : accum) xs
  go _ [] = internalError "Separated should not be empty"

consSeparated :: a -> SourceToken -> Separated a -> Separated a
consSeparated x sep (Separated {..}) = Separated x ((sep, sepHead) : sepTail)

internalError :: String -> a
internalError = error

toIdent :: SourceToken -> Ident
toIdent tok = case tok of
  (_, TokLowerName q a) -> Ident tok q a
  (_, TokUpperName q a) -> Ident tok q a
  (_, TokSymbol q a)    -> Ident tok q a
  (_, TokHole a)        -> Ident tok [] a
  _                     -> internalError $ "Invalid identifier token: " <> show tok

toVar :: SourceToken -> Ident
toVar tok = case tok of
  (_, TokLowerName q a)
    | not (Set.member a reservedNames) -> Ident tok q a
    | otherwise -> internalError $ "Unexpected keyword: " <> Text.unpack a
  _ -> internalError $ "Invalid variable token: " <> show tok

toSymbol :: SourceToken -> Ident
toSymbol tok = case tok of
  (_, TokSymbol q a)
    | not (Set.member a reservedSymbols) -> Ident tok q a
    | otherwise -> internalError $ "Unexpected reserved symbol: " <> Text.unpack a
  _ -> internalError $ "Invalid operator token: " <> show tok

toLabel :: SourceToken -> Ident
toLabel tok = case tok of
  (_, TokLowerName [] a) -> Ident tok [] a
  (_, TokLowerName q  _) -> internalError $ "Unexpected qualification: " <> show q
  (_, TokString _ a)     -> Ident tok [] a
  (_, TokRawString a)    -> Ident tok [] a
  _                      -> internalError $ "Invalid label: " <> show tok

toString :: SourceToken -> (SourceToken, Text)
toString tok = case tok of
  (_, TokString _ a)  -> (tok, a)
  (_, TokRawString a) -> (tok, a)
  _                   -> internalError $ "Invalid string literal: " <> show tok

toChar :: SourceToken -> (SourceToken, Char)
toChar tok = case tok of
  (_, TokChar _ a) -> (tok, a)
  _                -> internalError $ "Invalid char literal: " <> show tok

toNumber :: SourceToken -> (SourceToken, Either Integer Double)
toNumber tok = case tok of
  (_, TokInt _ a)    -> (tok, Left a)
  (_, TokNumber _ a) -> (tok, Right a)
  _                  -> internalError $ "Invalid number literal: " <> show tok

toInt :: SourceToken -> (SourceToken, Integer)
toInt tok = case tok of
  (_, TokInt _ a)    -> (tok, a)
  _                  -> internalError $ "Invalid integer literal: " <> show tok

toBoolean :: SourceToken -> (SourceToken, Bool)
toBoolean tok = case tok of
  (_, TokLowerName [] "true")  -> (tok, True)
  (_, TokLowerName [] "false") -> (tok, False)
  _                            -> internalError $ "Invalid boolean literal: " <> show tok

toBinder :: Monoid a => Expr a -> Binder a
toBinder = \case
  ExprTyped a expr tok ty -> BinderTyped a (toBinder expr) tok ty
  other -> toBinder0 other

toBinder0 :: Monoid a => Expr a -> Binder a
toBinder0 = \case
  ExprOp a lhs op rhs | Ident _ [] name <- op, name /= "@" ->
    BinderOp a (toBinder lhs) op (toBinder rhs)
  expr@(ExprApp _ _ _) -> goApp mempty [] expr
  other -> toBinderAtom other
  where
  goApp ann args (ExprApp a next rhs) = goApp (a <> ann) (toBinder rhs : args) next
  goApp ann args (ExprConstructor a ident) = BinderConstructor (a <> ann) ident args
  goApp _ _ _ = internalError $ "Unexpected expression in binder position"

toBinderAtom :: Monoid a => Expr a -> Binder a
toBinderAtom = \case
  -- TODO: Nested named patterns `a@b@c`
  ExprOp a1 (ExprIdent a2 name@(Ident _ [] _)) (Ident op [] "@") rhs ->
    BinderNamed (a1 <> a2) name op (toBinderAtom rhs)
  ExprSection a tok -> BinderWildcard a tok
  ExprIdent a ident@(Ident _ [] _) -> BinderVar a ident
  ExprChar a tok ch -> BinderChar a tok ch
  ExprString a tok str -> BinderString a tok str
  ExprNumber a tok num -> BinderNumber a tok num
  ExprArray a delim -> BinderArray a (fmap (fmap toBinder) <$> delim)
  ExprRecord a delim -> BinderRecord a (fmap (fmap (fmap toBinder)) <$> delim)
  ExprParens a wrap -> BinderParens a (toBinder <$> wrap)
  _ -> internalError $ "Unexpected expression in binder position"

toModuleDecls :: [Either (ImportDecl a) (Declaration a)] -> ([ImportDecl a], [Declaration a])
toModuleDecls = goLeft []
  where
  goLeft acc (Left x : xs) = goLeft (x : acc) xs
  goLeft acc xs = (reverse acc, goRight [] xs)

  goRight acc [] = reverse acc
  goRight acc (Right x : xs) = goRight (x : acc) xs
  goRight _ (Left _ : _) = internalError $ "Import declaration must appear at the start of a module"

varToType :: Monoid a => TypeVarBinding a -> Type a
varToType (TypeVarKinded (Wrapped l (Labeled var tok kind) r)) =
  TypeParens mempty
    (Wrapped l
      (TypeKinded mempty
        (TypeVar mempty var) tok kind) r)
varToType (TypeVarName ident) =
  TypeVar mempty ident

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
  , "kind"
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

reservedSymbols :: Set Text
reservedSymbols = Set.fromList
  [ "∀"
  ]
