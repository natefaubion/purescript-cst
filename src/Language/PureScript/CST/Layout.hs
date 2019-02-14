module Language.PureScript.CST.Layout where

import Prelude

import Data.DList (snoc)
import qualified Data.DList as DList
import Language.PureScript.CST.Types

data Layout
  = LytParen
  | LytBrace
  | LytSquare
  | LytCase
  | LytRoot
  | LytIndent !LayoutTerm
  deriving (Show, Eq)

data LayoutTerm
  = LytLet
  | LytClass
  | LytWhere
  | LytOf
  | LytArr
  | LytDo
  | LytAdo
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

endToken :: LayoutTerm -> Maybe Token
endToken = \case
  LytArr   -> Nothing
  LytClass -> Nothing
  _        -> Just TokLayoutEnd

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
    -- TODO
    ((_, LytIndent LytClass), TokLowerName [] "where") ->
      k2 (tail stk) acc
    -- When we encounter an `in`, we need to collapse layout until we hit a
    -- corresponding `let` or `ado`. This is to handle potentially silly
    -- edge cases like:
    --     let foo = do do do bar in foo
    ((_, LytIndent _), TokLowerName [] "in")
      | ((_, LytIndent lyt) : stk', acc') <- collapse inP stk acc ->
          k3 stk' $ acc' `snocEnd` lyt
      where
      inP LytLet _ = False
      inP LytAdo _ = False
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
      whereP LytDo _  = True
      whereP _ lytPos = srcColumn tokPos <= srcColumn lytPos
    -- TODO
    ((_, LytCase), TokLowerName [] "of") ->
      k2 (tail stk) acc
    -- TODO
    ((lytPos, LytIndent LytOf), TokRightArrow _) ->
      k2 ((lytPos, LytIndent LytArr) : stk) acc
    -- TODO
    -- ((_, LytIndent LytArr), TokPipe) ->
    --   k2 (tail stk) acc
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
      symbolP _ lytPos = srcColumn tokPos <= srcColumn lytPos
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
    -- Commas are a layout lexeme, but only under certain contexts. We need
    -- to track `case` arrows so that we can ignore the rule in multi-case
    -- patterns and guards.
    ((_, LytIndent LytArr), TokComma) ->
      uncurry k3 $ collapse trueP stk acc
    ((_, LytIndent LytDo), TokComma) ->
      uncurry k3 $ collapse trueP stk acc
    ((_, LytIndent LytWhere), TokComma) ->
      uncurry k3 $ collapse trueP stk acc
    -- ((_, LytIndent LytWhere), TokComma)
    --   | (stk'@((_, lyt) : _), acc') <- collapse trueP stk acc
    --   , lyt `elem` [ LytParen, LytSquare, LytBrace ] ->
    --      k3 stk' acc'
      -- uncurry k3 $ collapse trueP stk acc
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
    -- In general we want to maintain the invariant the decreasing indentation
    -- will collapse layout.
    (_, _) ->
      uncurry k2 $ collapse colP stk acc
      where
      colP  _ lytPos = srcColumn tokPos < srcColumn lytPos

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
            (LytArr, _) -> k3 (tail stk) $ acc `snoc` lytToken tokPos TokLayoutSep
            (_, _) -> k3 stk $ acc `snoc` lytToken tokPos TokLayoutSep
    _ -> k3 stk acc

  -- Insert the current token.
  k3 stk acc = case tok of
    -- TODO
    TokLowerName [] "class" | (_, LytIndent LytWhere) <- head stk ->
      k4 ((tokPos, LytIndent LytClass) : stk) $ acc `snoc` src
    -- TODO
    TokLowerName [] "case" ->
      k4 ((tokPos, LytCase) : stk) $ acc `snoc` src
    _ ->
      k4 stk $ acc `snoc` src

  -- Handle new layout contexts.
  k4 stk acc = case startsLayout tok of
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

  collapse p = go
    where
    go ((lytPos, LytIndent lyt) : stk) acc
      | p lyt lytPos = go stk $ acc `snocEnd` lyt
    go stk acc = (stk, acc)

  snocEnd acc lyt = case endToken lyt of
    Just t  -> acc `snoc` lytToken tokPos t
    Nothing -> acc

  trueP _ _ = True

unwindLayout :: SourcePos -> LayoutStack -> [SourceToken]
unwindLayout pos = go
  where
  go [] = []
  go ((_, LytRoot) : _) = [lytToken pos TokEof]
  go ((_, LytIndent lyt) : stk)
    | Just tok <- endToken lyt = lytToken pos tok : go stk
  go (_ : stk) = go stk
