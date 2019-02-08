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
  _                       -> []

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
  -- Insert TokLayoutEnds if the column is less than the current indent
  k1 $ collapse ((srcColumn tokPos <) . srcColumn) stack mempty
  where
  tokPos = srcStart $ tokRange tokAnn
  hasLyt = startsLayout tok
  endLyt = endsLayout tok

  k1 (stk, acc) = case (head stk, endLyt) of
    ((lytPos, LytIndent _), [])
      -- If this token starts a new layout, and the new layout column is
      -- the same as the current layout column, close the current layout.
      | Just (LytIndent _) <- hasLyt, srcColumn nextPos == srcColumn lytPos ->
          k2 (tail stk) $ acc `snoc` lytToken tokPos TokLayoutEnd
      -- If the column is the same as the current indent, but not the column,
      -- insert a separator. If the line is also the same, that means it's the
      -- first token in the layout, so a separator is unnecessary.
      | srcColumn tokPos == srcColumn lytPos && srcLine tokPos /= srcLine lytPos ->
          case tok of
            -- If the token is a symbol, close the layout. Eg. This would mean a binary
            -- operator would capture a do block on the lhs if the symbol is the
            -- same indentation as the block.
            TokSymbol _ _ -> k2 (tail stk) $ acc `snoc` lytToken tokPos TokLayoutEnd
            -- Otherwise insert a separator
            _ -> k2 stk $ acc `snoc` lytToken tokPos TokLayoutSep
    -- If the current token closes the current layout context, insert TokLayoutEnd.
    ((_, LytIndent la), cs) | LytIndent la `elem` cs ->
      k2 (tail stk) $ acc `snoc` lytToken tokPos TokLayoutEnd
    -- If we have a closing delimiter and are in a layout context, we need to
    -- insert TokLayoutEnds.
    ((_, LytIndent _), [delim'])
      | ((_, delim) : stk', acc') <- collapse (const True) stk acc
      , delim' == delim ->
          k2 stk' acc'
    -- Pop a matched closing delimiter
    ((_, delim), [delim']) | delim' == delim ->
      k2 (tail stk) acc
    (_, _) ->
      k2 stk acc

  -- Insert the current token
  k2 stk acc =
    k3 stk $ acc `snoc` src

  k3 stk acc = case hasLyt of
    -- If the token starts layout, we need to push a new layout context with
    -- the next token's position.
    Just (LytIndent la) ->
      k4 ((nextPos, LytIndent la) : stk) $
        acc `snoc` lytToken nextPos TokLayoutStart
    -- Push a new delimiter context.
    Just delim ->
      k4 ((tokPos, delim) : stk) acc
    Nothing ->
      k4 stk acc

  k4 stk acc =
    (stk, DList.toList acc)

  collapse p = go
    where
    go ((lytPos, LytIndent _) : stk) acc
      | p lytPos = go stk $ acc `snoc` lytToken tokPos TokLayoutEnd
    go stk acc = (stk, acc)

unwindLayout :: SourcePos -> LayoutStack -> [SourceToken]
unwindLayout pos = go
  where
  go [] = []
  go ((_, LytIndent LytRoot) : _) = [lytToken pos TokEof]
  go ((_, LytIndent _) : stk) = lytToken pos TokLayoutEnd : go stk
  go (_ : stk) = go stk
