module Language.PureScript.CST.Layout where

import Prelude

import Data.DList (snoc)
import qualified Data.DList as DList
import Data.Function ((&))
import Language.PureScript.CST.Types

type LayoutStack = [(SourcePos, LayoutDelim)]

data LayoutDelim
  = LytRoot
  | LytTopDecl
  | LytTopDeclHead
  | LytDeclGuard
  | LytCase
  | LytCaseBinders
  | LytCaseGuard
  | LytLambdaBinders
  | LytParen
  | LytBrace
  | LytSquare
  | LytIf
  | LytThen
  | LytProperty
  | LytTick
  | LytLet
  | LytWhere
  | LytOf
  | LytDo
  | LytAdo
  deriving (Show, Eq, Ord)

isIndented :: LayoutDelim -> Bool
isIndented = \case
  LytLet   -> True
  LytWhere -> True
  LytOf    -> True
  LytDo    -> True
  LytAdo   -> True
  _        -> False

isTopDecl :: SourcePos -> LayoutStack -> Bool
isTopDecl tokPos = \case
  [(lytPos, LytWhere), (_, LytRoot)]
    | srcColumn tokPos == srcColumn lytPos -> True
  _ -> False

lytToken :: SourcePos -> Token -> SourceToken
lytToken pos = (ann,)
  where
  ann = TokenAnn
    { tokRange = SourceRange pos pos
    , tokLeadingComments = []
    , tokTrailingComments = []
    }

insertLayout :: SourceToken -> SourcePos -> LayoutStack -> (LayoutStack, [SourceToken])
insertLayout src@(tokAnn, tok) nextPos stack =
  DList.toList <$> insert (stack, mempty)
  where
  tokPos =
    srcStart $ tokRange tokAnn

  insert state@(stk, acc) = case tok of
    TokLowerName [] "data" ->
      case state & insertDefault of
        state'@(stk', _) | isTopDecl tokPos stk' ->
          state' & pushStack tokPos LytTopDecl
        state' -> state' & popStack (== LytProperty)

    TokLowerName [] "foreign" ->
      case state & insertDefault of
        state'@(stk', _) | isTopDecl tokPos stk' ->
          state' & pushStack tokPos LytTopDecl
        state' -> state' & popStack (== LytProperty)

    TokLowerName [] "class" ->
      case state & insertDefault of
        state'@(stk', _) | isTopDecl tokPos stk' ->
          state' & pushStack tokPos LytTopDeclHead
        state' -> state' & popStack (== LytProperty)

    TokLowerName [] "where" ->
      case stk of
        (_, LytTopDeclHead) : stk' ->
          (stk', acc) & insertToken src & insertStart LytWhere
        (_, LytRoot) : _ ->
          state & insertToken src & insertStart LytWhere
        (_, lyt) : _ | isIndented lyt ->
          state & collapse whereP & insertToken src & insertStart LytWhere
        _ ->
          state & insertDefault & popStack (== LytProperty)
      where
      whereP _      LytDo = True
      whereP lytPos lyt   = offsideEndP lytPos lyt

    TokLowerName [] "in" ->
      case collapse inP state of
        ((_, LytLet) : (_, LytAdo) : stk', acc') ->
          (stk', acc') & insertEnd & insertEnd & insertToken src
        ((_, lyt) : stk', acc') | isIndented lyt ->
          (stk', acc') & insertEnd & insertToken src
        _ ->
          state & insertDefault & popStack (== LytProperty)
      where
      inP _ LytLet = False
      inP _ LytAdo = False
      inP _ lyt    = isIndented lyt

    TokLowerName [] "let" ->
      case state & insertDefault of
        ((_, LytProperty) : stk', acc') ->
          (stk', acc')
        state' ->
          state' & insertStart LytLet

    TokLowerName _ "do" ->
      case state & insertDefault of
        ((_, LytProperty) : stk', acc') ->
          (stk', acc')
        state' ->
          state' & insertStart LytDo

    TokLowerName _ "ado" ->
      case state & insertDefault of
        ((_, LytProperty) : stk', acc') ->
          (stk', acc')
        state' ->
          state' & insertStart LytAdo

    TokLowerName [] "case" ->
      case state & insertDefault of
        ((_, LytProperty) : stk', acc') ->
          (stk', acc')
        state' ->
          state' & pushStack tokPos LytCase

    TokLowerName [] "of" ->
      case collapse indentedP state of
        ((_, LytCase) : stk', acc') ->
          (stk', acc') & insertToken src & insertStart LytOf & pushStack nextPos LytCaseBinders
        state' ->
          state' & insertDefault & popStack (== LytProperty)

    TokLowerName [] "if" ->
      case state & insertDefault of
        ((_, LytProperty) : stk', acc') ->
          (stk', acc')
        state' ->
          state' & pushStack tokPos LytIf

    TokLowerName [] "then" ->
      case state & collapse indentedP of
        ((_, LytIf) : stk', acc') ->
          (stk', acc') & insertToken src & pushStack tokPos LytThen
        _ ->
          state & insertDefault & popStack (== LytProperty)

    TokLowerName [] "else" ->
      case state & collapse indentedP of
        ((_, LytThen) : stk', acc') ->
          (stk', acc') & insertToken src
        _ ->
          state & insertDefault & popStack (== LytProperty)

    TokBackslash ->
      state & insertDefault & pushStack tokPos LytLambdaBinders

    TokRightArrow _ ->
      state & collapse arrowP & popStack guardP & insertToken src
      where
      arrowP _      LytDo     = True
      arrowP _      LytOf     = False
      arrowP lytPos lyt       = offsideEndP lytPos lyt

      guardP LytCaseBinders   = True
      guardP LytCaseGuard     = True
      guardP LytLambdaBinders = True
      guardP _                = False

    TokEquals ->
      case stk of
        (_, LytLet) : _ ->
          state & insertToken src
        (_, LytWhere) : _ ->
          state & insertToken src
        _ ->
          case state & collapse equalsP of
            ((_, LytDeclGuard) : stk', acc') ->
              (stk', acc') & insertToken src
            _ ->
              state & insertDefault
      where
      equalsP _ LytWhere = True
      equalsP _ LytLet   = True
      equalsP _ _        = False

    TokPipe ->
      case collapse offsideEndP state of
        state'@((_, LytOf) : _, _) ->
          state' & pushStack tokPos LytCaseGuard & insertToken src
        state'@((_, LytLet) : _, _) ->
          state' & pushStack tokPos LytDeclGuard & insertToken src
        state'@((_, LytWhere) : _, _) ->
          state' & pushStack tokPos LytDeclGuard & insertToken src
        _ ->
          state & insertDefault

    TokTick ->
      case state & collapse indentedP of
        ((_, LytTick) : stk', acc') ->
          (stk', acc') & insertToken src
        _ ->
          state & insertDefault & pushStack tokPos LytTick

    TokComma ->
      case state & collapse indentedP of
        ([(_, LytRoot)], _) ->
          state & insertDefault
        state'@((_, LytBrace) : _, _) ->
          state' & insertToken src & pushStack tokPos LytProperty
        state' ->
          state' & insertToken src

    TokDot ->
      state & insertDefault & pushStack tokPos LytProperty

    TokLeftParen ->
      state & insertDefault & pushStack tokPos LytParen

    TokLeftBrace ->
      state & insertDefault & pushStack tokPos LytBrace & pushStack tokPos LytProperty

    TokLeftSquare ->
      state & insertDefault & pushStack tokPos LytSquare

    TokRightParen ->
      state & collapse indentedP & popStack (== LytParen) & insertToken src

    TokRightBrace ->
      state & collapse indentedP & popStack (== LytProperty) & popStack (== LytBrace) & insertToken src

    TokRightSquare ->
      state & collapse indentedP & popStack (== LytSquare) & insertToken src

    TokString _ _ ->
      state & insertDefault & popStack (== LytProperty)

    TokLowerName [] _ ->
      state & insertDefault & popStack (== LytProperty)

    TokSymbol _ _ ->
      state & collapse offsideEndP & insertSep & insertToken src

    _ ->
      state & insertDefault

  insertDefault state =
    state & collapse offsideP & insertSep & insertToken src

  insertStart lyt state =
    state & pushStack nextPos lyt & insertToken (lytToken nextPos TokLayoutStart)

  insertSep state@(stk, acc) = case stk of
    (lytPos, LytTopDecl) : stk' | sepP lytPos ->
      (stk', acc) & insertToken sepTok
    (lytPos, LytTopDeclHead) : stk' | sepP lytPos ->
      (stk', acc) & insertToken sepTok
    (lytPos, lyt) : _ | indentSepP lytPos lyt ->
      case lyt of
        LytOf -> state & insertToken sepTok & pushStack tokPos LytCaseBinders
        _     -> state & insertToken sepTok
    _ -> state
    where
    sepTok = lytToken tokPos TokLayoutSep

  insertEnd =
    insertToken (lytToken tokPos TokLayoutEnd)

  insertToken token (stk, acc) =
    (stk, acc `snoc` token)

  pushStack lytPos lyt (stk, acc) =
    ((lytPos, lyt) : stk, acc)

  popStack p ((_, lyt) : stk', acc)
    | p lyt = (stk', acc)
  popStack _ state = state

  collapse p = uncurry go
    where
    go ((lytPos, lyt) : stk) acc
      | p lytPos lyt =
          go stk $ if isIndented lyt
                   then acc `snoc` lytToken tokPos TokLayoutEnd
                   else acc
    go stk acc = (stk, acc)

  indentedP =
    const isIndented

  offsideP lytPos lyt =
    isIndented lyt && srcColumn tokPos < srcColumn lytPos

  offsideEndP lytPos lyt =
    isIndented lyt && srcColumn tokPos <= srcColumn lytPos

  indentSepP lytPos lyt =
    isIndented lyt && sepP lytPos

  sepP lytPos =
    srcColumn tokPos == srcColumn lytPos && srcLine tokPos /= srcLine lytPos

unwindLayout :: SourcePos -> LayoutStack -> [SourceToken]
unwindLayout pos = go
  where
  go [] = []
  go ((_, LytRoot) : _) = [lytToken pos TokEof]
  go ((_, lyt) : stk) | isIndented lyt = lytToken pos TokLayoutEnd : go stk
  go (_ : stk) = go stk
