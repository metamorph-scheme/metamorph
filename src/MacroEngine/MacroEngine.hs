module MacroEngine.MacroEngine where

import Parser.MetaNode
import qualified Data.Map as Map
import Data.List

type PatternNode = MetaNode
type Literal = String
type Ellipsis = String

type BindingTree = Maybe PatternRoot
type PatternRoot = [Pattern]
-- tree like structure for bindings
data Pattern = Ellipsis MetaNode [Pattern] | Value MetaNode MetaNode | EllipsisValue MetaNode | EllipsisSubPattern PatternRoot | Empty deriving (Show, Eq)

resolve :: MetaNode -> MetaNode -> MetaNode
resolve = error "not implemented"

-- verify if macro call matches a pattern
matchApplication :: [Literal] -> Ellipsis -> MetaNode -> PatternNode -> BindingTree
-- pattern has to be in scheme form for matchList to operate recursively
-- it can be an application node or a pair node (top-level pattern has to be of list form)
matchApplication literal ellipsis params@(ApplicationNode _ _) pattern
  | (ApplicationNode x xs) <- pattern = matchList literal ellipsis params (ApplicationNode (IdentifierAtom "_") xs)
  | (PairNode car cdr) <- pattern = matchList literal ellipsis params (PairNode (IdentifierAtom "_") cdr)
  | otherwise = error "top level pattern is not in list form"
matchApplication _ _ _ _ = error "invalid macro application passed to macro engine"

zipAndCombineBindings :: (MetaNode -> PatternNode -> BindingTree) -> [MetaNode] -> [PatternNode] -> BindingTree
-- zipWith yields [BindingTree] = [Maybe [Pattern]]
zipAndCombineBindings f el pl = combineBindings . zipWith f el $ pl

zipAndCombineEllipsisBindings :: (MetaNode -> PatternNode -> BindingTree) -> [MetaNode] -> [PatternNode] -> BindingTree
zipAndCombineEllipsisBindings f el pl = combineBindings . map (fmap patternListToEllipsisSubNode) . zipWith f el $ pl


patternListToEllipsisSubNode patternList
  | length filteredPatternList <= 1 = map valueToEllipsisValue filteredPatternList
  | otherwise = [EllipsisSubPattern filteredPatternList]
  where filteredPatternList = filter isNotEmpty patternList

valueToEllipsisValue (Value _ v) = (EllipsisValue v)
valueToEllipsisValue pattern = pattern

isNotEmpty :: Pattern -> Bool
isNotEmpty Empty = False
isNotEmpty _ = True

-- Maybe PatternRoot
combineBindings :: [BindingTree] -> BindingTree
combineBindings [] = Just [] -- changed from Just emptyTree
combineBindings l = foldl1 mergeTrees l
  where mergeTrees acc x = treeUnionWith <$> acc <*> x
  -- where mergeMaps = (<*>) . (<$>) $ Map.unionWith (error "duplicate pattern variable")


treeUnionWith :: PatternRoot -> PatternRoot -> PatternRoot
treeUnionWith a b = nubBy isDuplicate . filter isNotEmpty $ (a ++ b)
  where isDuplicate (Value ia _) (Value ib _) = if ia == ib then (error "duplicate pattern variable") else False
        isDuplicate _ _ = False


emptyTree :: PatternRoot
emptyTree = [Empty]

singletonTree :: MetaNode -> MetaNode -> PatternRoot
singletonTree pattern binding = [Value pattern binding]

ellipsisTree :: MetaNode -> [Pattern] -> PatternRoot
ellipsisTree pattern bindingList = [Ellipsis pattern bindingList]

-- combineBindings :: [[BindingMap]] -> [BindingMap]


match :: [Literal] -> Ellipsis -> MetaNode -> PatternNode -> BindingTree
-- if p is an underscore
match _ _ _ (IdentifierAtom "_") = Just emptyTree
-- if p is non literal
match literals _ e@(IdentifierAtom identifier) p@(IdentifierAtom pidentifier)
  | pidentifier `elem` literals && identifier /= pidentifier = Nothing
  | otherwise = Just $ singletonTree p e

match literals _ e p@(IdentifierAtom pidentifier)
  | pidentifier `elem` literals = Nothing
  | otherwise = Just $ singletonTree p e

-- if p is list
match literals ellipsis list@(PairNode car cdr) plist@(PairNode pcar pcdr) = matchList literals ellipsis list plist
match _ _ (NumberAtom num) (NumberAtom pnum) = whenMaybe emptyTree $ num == pnum
match _ _ (BoolAtom bool) (BoolAtom pbool) = whenMaybe emptyTree $ bool == pbool
match _ _ (CharAtom char) (CharAtom pchar) = whenMaybe emptyTree $ char == pchar
match _ _ (StringAtom str) (StringAtom pstr) = whenMaybe emptyTree $ str == pstr
-- otherwise does not match
match _ _ _ _ = Nothing

whenMaybe :: a -> Bool -> Maybe a
whenMaybe a True = Just a
whenMaybe _ False = Nothing

matchList :: [Literal] -> Ellipsis -> MetaNode -> PatternNode -> BindingTree
matchList literals ellipsis params patterns
 | ellipsisOccurences > 1 = error "illegal ellipsis in pattern"
 | ellipsisOccurences == 1 = if properLenght then (combineBindings [headMatches, ellipsisMatches, tailMatches]) else Nothing
 | otherwise = if (length patternList == paramLenght) then (zipAndCombineBindings (match literals ellipsis) paramList patternList) else Nothing
  where
    properLenght = patternLenght <= paramLenght
    headMatches = if (headLenght == length headParams) then (zipAndCombineBindings (match literals ellipsis) headParams headPattern) else Nothing
    ellipsisMatches = fmap (\patternList -> ellipsisTree ellipsisPattern patternList) ellipsisBindingTree
    ellipsisBindingTree = zipAndCombineEllipsisBindings (match literals ellipsis) ellipsisParams (repeat ellipsisPattern)
    tailMatches = if (tailLenght == length tailParams) then (zipAndCombineBindings (match literals ellipsis) tailParams tailPattern) else Nothing 
    headParams = take headLenght paramList
    tailParams = drop (paramLenght - tailLenght) paramList
    ellipsisParams = take ellipsisParamsLenght . drop headLenght $ paramList
    ellipsisParamsLenght = paramLenght - tailLenght - headLenght
    patternLenght = headLenght + tailLenght
    (headLenght, tailLenght) = mapTuple length (headPattern, tailPattern)
    (headPattern, ellipsisPattern) = (init tHeadPattern, last tHeadPattern)
    splitPattern@(tHeadPattern, tailPattern) = splitAtFirst ellipsisNode patternList
    patternList = makeList patterns
    paramLenght = length paramList
    paramList = makeList params
    ellipsisOccurences = count ellipsisNode patternList
    ellipsisNode = IdentifierAtom ellipsis
    

makeList :: MetaNode -> [MetaNode]
makeList (ApplicationNode x xs) = x : xs
makeList (PairNode car cdr) = car : (makeList cdr)
-- handle proper and improper lists the same way
makeList (EmptyAtom) = []
makeList node = [node]

-- split list at ellipsis
splitAtFirst :: Eq a => a -> [a] -> ([a], [a])
splitAtFirst x = fmap (drop 1) . break (x ==)

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a1, a2) = (f a1, f a2)

count   :: Eq a => a -> [a] -> Int
count x =  length . filter (==x)