module Language.PureScript.CST.Layout where

import Prelude

import Data.DList (snoc)
import qualified Data.DList as DList
import Language.PureScript.CST.Types

data Layout
  = LytRoot
  | LytParen
  | LytBrace
  | LytSquare
  | LytTick
  | LytCase
  | LytCaseGuard
  | LytDeclGuard
  | LytLambda
  | LytDecl
  | LytDeclWhere
  | LytIndent !LayoutTerm
  deriving (Show, Eq)

data LayoutTerm
  = LytLet
  | LytWhere
  | LytOf
  | LytDo
  | LytAdo
  deriving (Show, Eq)

type LayoutStack = [(SourcePos, Layout)]

startsLayout :: LayoutStack -> Token -> Maybe Layout
startsLayout stk = \case
  TokLeftParen            -> Just LytParen
  TokLeftBrace            -> Just LytBrace
  TokLeftSquare           -> Just LytSquare
  TokBackslash            -> Just LytLambda
  TokLowerName [] "case"  -> Just LytCase
  TokLowerName [] "let"   -> Just (LytIndent LytLet)
  TokLowerName [] "where" -> Just (LytIndent LytWhere)
  TokLowerName [] "of"    -> Just (LytIndent LytOf)
  TokLowerName _  "do"    -> Just (LytIndent LytDo)
  TokLowerName _  "ado"   -> Just (LytIndent LytAdo)
  TokLowerName [] "data"
    | [(_, LytIndent LytWhere), (_, LytRoot)] <- stk -> Just LytDecl
  TokLowerName [] "foreign"
    | [(_, LytIndent LytWhere), (_, LytRoot)] <- stk -> Just LytDecl
  TokLowerName [] "class"
    | [(_, LytIndent LytWhere), (_, LytRoot)] <- stk -> Just LytDeclWhere
  TokPipe
    | (_, LytIndent LytOf)    : _ <- stk -> Just LytCaseGuard
    | (_, LytIndent LytLet)   : _ <- stk -> Just LytDeclGuard
    | (_, LytIndent LytWhere) : _ <- stk -> Just LytDeclGuard
  _ -> Nothing

lytToken :: SourcePos -> Token -> SourceToken
lytToken pos = (ann,)
  where
  ann = TokenAnn
    { tokRange = SourceRange pos pos
    , tokLeadingComments = []
    , tokTrailingComments = []
    }

insertLayout :: SourceToken -> SourcePos -> LayoutStack -> (LayoutStack, [SourceToken])
insertLayout src@(tokAnn, tok) nextPos stack = k1 stack mempty
  where
  tokPos = srcStart $ tokRange tokAnn

  k1 stk acc = case (head stk, tok) of
    -- As part of comma-masking in class declarations, we need to remove the
    -- mask when we encounter a `where`. Class declarations don't need a
    -- `where` delimiter if there are no members, but that case is handled
    -- where separators are inserted.
    ((_, LytDeclWhere), TokLowerName [] "where") ->
      k2 (tail stk) acc
    -- When we encounter an `in`, we need to collapse layout until we hit a
    -- corresponding `let` or `ado`. This is to handle potentially silly
    -- edge cases like:
    --     let foo = do do do bar in foo
    ((_, LytIndent _), TokLowerName [] "in")
      | ((_, LytIndent LytLet) : (_, LytIndent LytAdo) : stk', acc') <- collapse inP stk acc ->
          k3 stk' $ acc' `snoc` lytToken tokPos TokLayoutEnd `snoc` lytToken tokPos TokLayoutEnd
      | ((_, LytIndent _) : stk', acc') <- collapse inP stk acc ->
          k3 stk' $ acc' `snoc` lytToken tokPos TokLayoutEnd
      where
      inP _ LytLet = False
      inP _ LytAdo = False
      inP _ _ = True
    -- When we encounter a `where`, we collapse any layout that is less than
    -- or equal to the token's column. This is so we can handle `case` and
    -- `do` like:
    --     test = case a of
    --       Foo foo -> do
    --         foo
    --       where
    --         a = Foo 42
    ((_, LytIndent _), TokLowerName [] "where") ->
      uncurry k3 $ collapse whereP stk acc
      where
      whereP _ LytDo  = True
      whereP lytPos _ = srcColumn tokPos <= srcColumn lytPos
    -- As part of comma-masking in `case` heads, we need to remove the mask
    -- when we encounter `of`.
    (_, TokLowerName [] "of")
      | ((_, LytCase) : stk', acc') <- collapse trueP stk acc ->
          k3 stk' acc'
    ((_, LytLambda), TokRightArrow _) ->
      k3 (tail stk) acc

    ((_, LytCaseGuard), TokRightArrow _) ->
      k3 (tail stk) acc
    (_, TokRightArrow _)
      | ((_, LytCaseGuard) : stk', acc') <- collapse ofP stk acc ->
          k3 stk' acc'
      where
      ofP _ LytOf = False
      ofP _ _     = True

    ((_, LytIndent LytLet), TokEquals) ->
      k3 stk acc
    ((_, LytIndent LytWhere), TokEquals) ->
      k3 stk acc
    ((_, LytDeclGuard), TokEquals) ->
      k3 (tail stk) acc
    (_, TokEquals)
      | ((_, LytDeclGuard) : stk', acc') <- collapse declP stk acc ->
          k3 stk' acc'
      where
      declP _ LytLet   = True
      declP _ LytWhere = True
      declP _ _        = False
    -- When we encounter a symbol in a do-block where we would usually insert
    -- a separator, we pre-emptively close the layout. This means that an
    -- operator can capture a do-block on the lhs in this case:
    --     foo = do
    --       this <- bar
    --       that do
    --         bar
    --       <|> baz
    ((_, LytIndent _), TokSymbol _ _) ->
      uncurry k3 $ collapse symbolP stk acc
      where
      symbolP lytPos _ = srcColumn tokPos <= srcColumn lytPos
    -- When we encounter a right delim, we need to collapse layouts. This is
    -- so we can write expressions like:
    --     foo = (do
    --       this <- bar
    --       that) == baz
    ((_, LytIndent _), TokRightParen)
      | ((_, LytParen) : stk', acc') <- collapse trueP stk acc ->
          k3 stk' acc'
    ((_, LytIndent _), TokRightBrace)
      | ((_, LytBrace) : stk', acc') <- collapse trueP stk acc ->
          k3 stk' acc'
    ((_, LytIndent _), TokRightSquare)
      | ((_, LytSquare) : stk', acc') <- collapse trueP stk acc ->
          k3 stk' acc'
    ((_, LytIndent _), TokTick)
      | ((_, LytTick) : stk', acc') <- collapse trueP stk acc ->
          k3 stk' acc'



    (_, TokComma) ->
      uncurry k3 $ collapse trueP stk acc
    -- We need to pop matching delimiters from the stack. We skip straight to
    -- k4 so that we don't insert separators in cases like:
    --     foo = do
    --       this <- bar
    --       that (
    --         baz
    --       )
    --       those
    ((_, LytParen), TokRightParen) ->
      k3 (tail stk) acc
    ((_, LytBrace), TokRightBrace) ->
      k3 (tail stk) acc
    ((_, LytSquare), TokRightSquare) ->
      k3 (tail stk) acc
    ((_, LytTick), TokTick) ->
      k3 (tail stk) acc
    -- We need to insert a LytTick point here instead of `startsLayout` because
    -- it's a single delimiter that opens and closes, rather than a pair. If we
    -- added it to `startsLayout`, then it would insert a LytTick even when it
    -- was meant to close the context.
    (_, TokTick)
      | ((_, LytTick) : stk', acc') <- collapse trueP stk acc ->
          k3 stk' acc'
      | otherwise ->
          k3 ((tokPos, LytTick) : stk) acc
    -- In general we want to maintain the invariant the decreasing indentation
    -- will collapse layout.
    (_, _) ->
      uncurry k2 $ collapse colP stk acc
      where
      colP lytPos _ = srcColumn tokPos < srcColumn lytPos

  -- If the current token is at the same indentation level as the current
  -- layout context, then we should insert a separator.
  k2 stk acc = case head stk of
    (lytPos, LytIndent lyt)
      | srcColumn tokPos == srcColumn lytPos && srcLine tokPos /= srcLine lytPos ->
          case (lyt, tok) of
            -- We make allowances for `then` and `else` so that they can be used within
            -- `do` syntax without stair-stepping indentation.
            (LytDo, TokLowerName [] "then") -> k3 stk acc
            (LytDo, TokLowerName [] "else") -> k3 stk acc
            (LytOf, _) -> k3 ((lytPos, LytCaseGuard) : stk) $ acc `snoc` lytToken tokPos TokLayoutSep
            (_, _) -> k3 stk $ acc `snoc` lytToken tokPos TokLayoutSep
    (lytPos, LytDeclWhere)
      | srcColumn tokPos <= srcColumn lytPos ->
          k3 (tail stk) $ acc `snoc` lytToken tokPos TokLayoutSep
    (lytPos, LytDecl)
      | srcColumn tokPos <= srcColumn lytPos ->
          k3 (tail stk) $ acc `snoc` lytToken tokPos TokLayoutSep
    _ -> k3 stk acc

  -- Insert the current token.
  k3 stk acc =
    k4 stk $ acc `snoc` src

  -- Handle new layout contexts.
  k4 stk acc = case startsLayout stk tok of
    Just (LytIndent LytOf) ->
      k5 ((nextPos, LytCaseGuard) : (nextPos, LytIndent LytOf) : stk) $
        acc `snoc` lytToken nextPos TokLayoutStart
    Just LytCaseGuard
      | ((lytPos, LytCaseGuard) : stk', acc') <- collapse ofP stk acc ->
          k5 ((lytPos, LytCaseGuard) : stk') acc'
      | ((lytPos, LytIndent LytOf) : stk', acc') <- collapse ofP stk acc ->
          k5 ((lytPos, LytCaseGuard) : (lytPos, LytIndent LytOf) : stk') acc'
      where
      ofP _ LytOf = False
      ofP _ _     = True
    -- If the token starts layout, we need to push a new layout context with
    -- the next token's position.
    Just (LytIndent la) ->
      k5 ((nextPos, LytIndent la) : stk) $
        acc `snoc` lytToken nextPos TokLayoutStart
    Just delim ->
      k5 ((tokPos, delim) : stk) acc
    Nothing ->
      k5 stk acc

  k5 stk acc =
    (stk, DList.toList acc)

  collapse p = collapseWhile go
    where
    go lytPos (LytIndent lyt) = p lytPos lyt
    go _ _ = False

  collapseWhile p = go
    where
    go ((lytPos, lyt) : stk) acc
      | p lytPos lyt = go stk $ case lyt of
          LytIndent _ -> acc `snoc` lytToken tokPos TokLayoutEnd
          _           -> acc
    go stk acc = (stk, acc)

  trueP _ _ = True

unwindLayout :: SourcePos -> LayoutStack -> [SourceToken]
unwindLayout pos = go
  where
  go [] = []
  go ((_, LytRoot) : _) = [lytToken pos TokEof]
  go ((_, LytIndent _) : stk) = lytToken pos TokLayoutEnd : go stk
  go (_ : stk) = go stk
