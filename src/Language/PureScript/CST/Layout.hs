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
  k1 $ collapse ((srcColumn tokPos <) . srcColumn) stack mempty
  where
  tokPos = srcStart $ tokRange tokAnn
  hasLyt = startsLayout tok
  endLyt = endsLayout tok

  k1 (stk, acc) = case (head stk, endLyt) of
    ((lytPos, LytIndent _), [])
      | Just (LytIndent _) <- hasLyt, srcColumn nextPos == srcColumn lytPos ->
          k2 (tail stk) $ acc `snoc` lytToken tokPos TokLayoutEnd
      | srcColumn tokPos == srcColumn lytPos && srcLine tokPos /= srcLine lytPos ->
          k2 stk $ acc `snoc` lytToken tokPos TokLayoutSep
    ((_, LytIndent la), cs) | LytIndent la `elem` cs ->
      k2 (tail stk) $ acc `snoc` lytToken tokPos TokLayoutEnd
    ((_, LytIndent _), [delim'])
      | ((_, delim) : stk', acc') <- collapse (const True) stk acc
      , delim' == delim ->
          k2 stk' acc'
    ((_, delim), [delim']) | delim' == delim ->
      k2 (tail stk) acc
    (_, _) ->
      k2 stk acc

  k2 stk acc =
    k3 stk $ acc `snoc` src

  k3 stk acc = case hasLyt of
    Just (LytIndent la) ->
      k4 ((nextPos, LytIndent la) : stk) $
        acc `snoc` lytToken nextPos TokLayoutStart
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
