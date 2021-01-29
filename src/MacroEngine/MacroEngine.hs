module MacroEngine.MacroEngine where

import Parser.MetaNode

type Pattern = MetaNode
type Literal = String
type Ellipsis = String

resolve :: MetaNode -> MetaNode -> MetaNode
resolve = error "not implemented"

-- verify if macro call matches a pattern
matchApplication :: MetaNode -> [Pattern] -> [Literal] -> Bool
matchApplication (ApplicationNode _ (_:params)) = matchParams params
matchApplication _ = error "invalid macro application passed to macro engine"

matchParams :: [MetaNode] -> [Pattern] -> [Literal] -> Bool
matchParams params patterns literals = and $ zipWith (match literals) params patterns

match :: [Literal] -> Ellipsis -> MetaNode -> Pattern -> Bool
match _ _ _ (IdentifierAtom "_") = True
match literals _ _ (IdentifierAtom identifier) = identifier `elem` literals
match literals ellipsis (PairNode car cdr) (PairNode pcar pcdr)
  | (IdentifierAtom ellipsis) <- pcar = True
  | otherwise = match literals ellipsis car pcar && match literals ellipsis cdr pcdr

matchList :: [Literal] -> Ellipsis -> MetaNode -> Pattern -> Maybe MetaNode -> Bool
matchList literals ellipsis (PairNode car cdr) (PairNode pcar pcdr) Nothing =
  match literals ellipsis car pcar && matchList literals ellipsis cdr pcdr (Just pcar) 
matchList literals ellipsis e@(PairNode car cdr) p@(PairNode pcar pcdr) (Just acc)
  | (IdentifierAtom ellipsis) <- pcar =
      if matchesAcc
        then matchesAcc && matchList literals ellipsis cdr p (Just acc)
        else matchList literals ellipsis e pcar
      where matchesAcc = match literals ellipsis car acc
  | otherwise = match literals ellipsis car pcar && matchList literals ellipsis cdr pcdr (Just acc)
