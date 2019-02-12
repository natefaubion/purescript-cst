module Language.PureScript.CST.Layout where

import Prelude

import Data.DList (snoc)
import qualified Data.DList as DList
import Language.PureScript.CST.Types

data Layout
  = LytParen
  | LytBrace
  | LytSquare
  | LytIndent !LayoutTerm
  deriving (Show, Eq)

data LayoutTerm
  = LytLet
  | LytWhere
  | LytOf
  | LytDo
  | LytAdo
  | LytRoot
  deriving (Show, Eq)

type LayoutStack = [(SourcePos, Layout)]

startsLayout :: Token -> Maybe Layout
startsLayout = \case
  TokLeftParen            -> Just LytParen
  TokLeftBrace            -> Just LytBrace
  TokLeftSquare           -> Just LytSquare
  TokLowerName [] "let"   -> Just (LytIndent LytLet)
  TokLowerName [] "where" -> Just (LytIndent LytWhere)
  TokLowerName [] "of"    -> Just (LytIndent LytOf)
  TokLowerName _ "do"     -> Just (LytIndent LytDo)
  TokLowerName _ "ado"    -> Just (LytIndent LytAdo)
  _                       -> Nothing

endsLayout :: Token -> [Layout]
endsLayout = \case
  TokRightParen           -> [LytParen]
  TokRightBrace           -> [LytBrace]
  TokRightSquare          -> [LytSquare]
  TokLowerName [] "in"    -> [LytIndent LytLet, LytIndent LytAdo]
  TokLowerName [] "where" -> [LytIndent LytOf, LytIndent LytDo]
  _ -> []

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
    -- When we encounter an `in`, we need to collapse layout until we hit a
    -- corresponding `let` or `ado`. This is to handle potentially silly
    -- edge cases like:
    --     let foo = do do do bar in foo
    ((_, LytIndent _), TokLowerName [] "in")
      -- This guards on a cons so that we don't walk all the way back up to
      -- the LytRoot, leaving the stack empty.
      | (_ : stk', acc') <- collapse inP stk acc ->
          k2 stk' $ acc' `snoc` lytToken tokPos TokLayoutEnd
      where
      inP LytLet _ = False
      inP LytAdo _ = False
      inP _ _ = True
    -- When we encounter a `where`, we collapse based on indentation, and if
    -- we are left with layout at the same column, we end it. This is to
    -- handle `case` and `do` like:
    --     test = case a of
    --       Foo foo -> foo
    --       where
    --         a = Foo 42
    ((_, LytIndent _), TokLowerName [] "where")
      | ((lytPos, LytIndent _) : stk', acc') <- collapse colP stk acc
      , srcColumn tokPos == srcColumn lytPos ->
          k2 stk' $ acc' `snoc` lytToken tokPos TokLayoutEnd
    -- When we encounter a 'where', we need to collapse all `do`s that are on
    -- the same line. This is to handle potentially silly edge cases like:
    --     foo = do do do bar where bar = 42
    ((_, LytIndent _), TokLowerName [] "where")
      | (stk', acc') <- collapse doP stk acc ->
          k2 stk' acc'
    -- When we encounter a symbol in a do-block where we would usually insert
    -- a separator, we pre-emptively close the layout. This means that an
    -- operator can capture a do-block on the lhs in this case:
    --     foo = do
    --       this <- bar
    --       that
    --       <|> baz
    ((lytPos, LytIndent LytDo), TokSymbol _ _)
      | srcColumn tokPos == srcColumn lytPos ->
          k2 (tail stk) $ acc `snoc` lytToken tokPos TokLayoutEnd
    -- When we encounter a right delim, we need to collapse layouts. This is
    -- so we can write expressions like:
    --     foo = (do
    --       this <- bar
    --       that) == baz
    ((_, LytIndent _), TokRightParen)
      | ((_, LytParen) : stk', acc') <- collapse trueP stk acc ->
          k2 stk' acc'
    ((_, LytIndent _), TokRightBrace)
      | ((_, LytBrace) : stk', acc') <- collapse trueP stk acc ->
          k2 stk' acc'
    ((_, LytIndent _), TokRightSquare)
      | ((_, LytSquare) : stk', acc') <- collapse trueP stk acc ->
          k2 stk' acc'
    -- We need to pop matching delimiters from the stack. We skip straight to
    -- k4 so that we don't insert separators in cases like:
    --     foo = do
    --       this <- bar
    --       that (
    --         baz
    --       )
    --       those
    ((_, LytParen), TokRightParen) ->
      k4 (tail stk) acc
    ((_, LytBrace), TokRightBrace) ->
      k4 (tail stk) acc
    ((_, LytSquare), TokRightSquare) ->
      k4 (tail stk) acc
    (_, _) ->
      k2 stk acc

  -- In general we want to maintain the invariant the decreasing indentation
  -- will collapse layout. We don't run this first because of the special
  -- handling around in/where and closing delimiters.
  k2 stk acc = uncurry k3 $ collapse colP stk acc

  -- If the current token is at the same indentation level as the current
  -- layout context, then we should insert a separator.
  k3 stk acc = case head stk of
    (lytPos, LytIndent _)
      | srcColumn tokPos == srcColumn lytPos && srcLine tokPos /= srcLine lytPos ->
          k4 stk $ acc `snoc` lytToken tokPos TokLayoutSep
    _ ->
      k4 stk acc

  -- Insert the current token.
  k4 stk acc = k5 stk $ acc `snoc` src

  -- Handle new layout contexts.
  k5 stk acc = case startsLayout tok of
    -- If the token starts layout, we need to push a new layout context with
    -- the next token's position.
    Just (LytIndent la) ->
      k6 ((nextPos, LytIndent la) : stk) $
        acc `snoc` lytToken nextPos TokLayoutStart
    Just delim ->
      k6 ((tokPos, delim) : stk) acc
    Nothing ->
      k6 stk acc

  k6 stk acc =
    (stk, DList.toList acc)

  collapse p = go
    where
    go ((lytPos, LytIndent ly) : stk) acc
      | p ly lytPos = go stk $ acc `snoc` lytToken tokPos TokLayoutEnd
    go stk acc = (stk, acc)

  trueP _ _      = True
  doP   l lytPos = l == LytDo && srcLine tokPos == srcLine lytPos
  colP  _ lytPos = srcColumn tokPos < srcColumn lytPos

unwindLayout :: SourcePos -> LayoutStack -> [SourceToken]
unwindLayout pos = go
  where
  go [] = []
  go ((_, LytIndent LytRoot) : _) = [lytToken pos TokEof]
  go ((_, LytIndent _) : stk) = lytToken pos TokLayoutEnd : go stk
  go (_ : stk) = go stk
