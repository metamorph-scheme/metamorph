module MacroEngine.MacroEngine where

import Parser.MetaNode
import qualified Data.Map as Map
import Data.List

type PatternNode = MetaNode
type Literal = String
type Ellipsis = String

-- tree like structure for bindings
type BindingTree = [Binding]
data Binding = Ellipsis MetaNode [Binding] | Value MetaNode MetaNode | EllipsisValue MetaNode | EllipsisSubPattern BindingTree | Empty deriving (Show, Eq)

-- structure for patterns and templates
data Pattern = Pattern { ellipsis :: Ellipsis, literals :: [Literal], patternNode :: PatternNode}
type Template = MetaNode
data SyntaxRule = SyntaxRule Pattern Template
type SyntaxRules = [SyntaxRule]


resolve :: MetaNode -> MetaNode -> MetaNode
resolve = error "not implemented"

parseSyntaxRules :: MetaNode -> SyntaxRules
-- syntax-rules without ellipsis argument
parseSyntaxRules (ApplicationNode (IdentifierAtom "syntax-rules") ((ApplicationNode literalsCar literalsCdr):syntaxRules)) =
  map (parseSyntaxRule "..." (map extractLiteral (literalsCar:literalsCdr))) syntaxRules
-- syntax-rules with ellipsis argument
parseSyntaxRules (ApplicationNode (IdentifierAtom "syntax-rules") ((IdentifierAtom ellipsis):(ApplicationNode literalsCar literalsCdr):syntaxRules)) =
  map (parseSyntaxRule ellipsis (map extractLiteral (literalsCar:literalsCdr))) syntaxRules
parseSyntaxRules (ApplicationNode (IdentifierAtom "syntax-rules") _) = error "invalid syntax-rules syntax"
parseSyntaxRules _ = error "macro transformer via syntax-rules expected"

extractLiteral :: MetaNode -> Literal
extractLiteral (IdentifierAtom name) = name
extractLiteral _ = error "invalid literal identifier given"

parseSyntaxRule :: Ellipsis -> [Literal] -> MetaNode -> SyntaxRule
parseSyntaxRule ellipsis literals (ApplicationNode patternNode [template]) =
  SyntaxRule Pattern{ellipsis=ellipsis, literals=literals, patternNode=patternNode} template
parseSyntaxRule _ _ _ = error "invalid syntax-rule syntax"


-- verify if macro call matches a pattern
matchApplication :: Pattern -> MetaNode -> Maybe BindingTree
-- pattern has to be in scheme form for matchList to operate recursively
-- it can be an application node or a pair node (top-level pattern has to be of list form)
matchApplication pattern@Pattern{patternNode=patternNode} params@(ApplicationNode _ _)
  | (ApplicationNode (IdentifierAtom _) xs) <- patternNode = matchList pattern{patternNode = (ApplicationNode (IdentifierAtom "_") xs)} params
  | (PairNode (IdentifierAtom _) cdr) <- patternNode = matchList pattern{patternNode = (PairNode (IdentifierAtom "_") cdr)} params
  | otherwise = error "top level pattern is not in list form"
matchApplication _ _ = error "invalid macro application passed to macro engine"

zipAndCombineBindings :: (PatternNode -> MetaNode -> Maybe BindingTree) -> [PatternNode] -> [MetaNode] -> Maybe BindingTree
-- zipWith yields [Maybe BindingTree] = [Maybe [Binding]]
zipAndCombineBindings f pl el = combineBindings . zipWith f pl $ el

zipAndCombineEllipsisBindings :: (PatternNode -> MetaNode -> Maybe BindingTree) -> [PatternNode] -> [MetaNode] -> Maybe BindingTree
zipAndCombineEllipsisBindings f pl el = combineBindings . map (fmap bindingListToEllipsisSubNode) . zipWith f pl $ el


bindingListToEllipsisSubNode bindingList
  | length filteredBindingList <= 1 = map valueToEllipsisValue filteredBindingList
  | otherwise = [EllipsisSubPattern filteredBindingList]
  where filteredBindingList = filter isNotEmpty bindingList

valueToEllipsisValue (Value _ v) = (EllipsisValue v)
valueToEllipsisValue pattern = pattern

isNotEmpty :: Binding -> Bool
isNotEmpty Empty = False
isNotEmpty _ = True

-- Maybe BindingTree
combineBindings :: [Maybe BindingTree] -> Maybe BindingTree
combineBindings [] = Just [] -- changed from Just emptyTree
combineBindings l = foldl1 mergeTrees l
  where mergeTrees acc x = treeUnionWith <$> acc <*> x
  -- where mergeMaps = (<*>) . (<$>) $ Map.unionWith (error "duplicate pattern variable")


treeUnionWith :: BindingTree -> BindingTree -> BindingTree
treeUnionWith a b = nubBy isDuplicate . filter isNotEmpty $ (a ++ b)
  where isDuplicate (Value ia _) (Value ib _) = if ia == ib then (error "duplicate pattern variable") else False
        isDuplicate _ _ = False


emptyTree :: BindingTree
emptyTree = [Empty]

singletonTree :: MetaNode -> MetaNode -> BindingTree
singletonTree pattern binding = [Value pattern binding]

ellipsisTree :: MetaNode -> [Binding] -> BindingTree
ellipsisTree pattern bindingList = [Ellipsis pattern bindingList]

curriedMatch :: Ellipsis -> [Literal] -> PatternNode -> MetaNode -> Maybe BindingTree
curriedMatch ellipsis literals pattern = match Pattern{ellipsis=ellipsis, literals=literals, patternNode=pattern}

match :: Pattern -> MetaNode -> Maybe BindingTree
-- if p is an underscore
match Pattern{patternNode = (IdentifierAtom "_")} _ = Just emptyTree
-- if p is non literal
match Pattern{literals = literals, patternNode = p@(IdentifierAtom pidentifier)} e@(IdentifierAtom identifier)
-- TODO discuss role of "lexical binding" in r7rs page 23, right column
  | pidentifier `elem` literals && identifier /= pidentifier = Nothing
  | otherwise = Just $ singletonTree p e

match Pattern{literals=literals, patternNode = p@(IdentifierAtom pidentifier)} e
  | pidentifier `elem` literals = Nothing
  | otherwise = Just $ singletonTree p e

-- if p is list
match pattern@Pattern{patternNode=plist@(PairNode _ _)} list@(PairNode _ _) = matchList pattern{patternNode=plist} list
match pattern@Pattern{patternNode=plist@(ApplicationNode _ _)} list@(ApplicationNode _ _) = matchList pattern{patternNode=plist} list
match Pattern{patternNode = (NumberAtom pnum)} (NumberAtom num) = whenMaybe emptyTree $ num == pnum
match Pattern{patternNode = (BoolAtom pbool)} (BoolAtom bool) = whenMaybe emptyTree $ bool == pbool
match Pattern{patternNode = (CharAtom pchar)} (CharAtom char) = whenMaybe emptyTree $ char == pchar
match Pattern{patternNode = (StringAtom pstr)} (StringAtom str) = whenMaybe emptyTree $ str == pstr
-- otherwise does not match
match _ _ = Nothing

whenMaybe :: a -> Bool -> Maybe a
whenMaybe a True = Just a
whenMaybe _ False = Nothing

-- TODO ellipsis and _ can be literals

matchList :: Pattern -> MetaNode -> Maybe BindingTree
matchList Pattern{ellipsis=ellipsis, literals=literals, patternNode=patternNode} params
 | ellipsisOccurences > 1 = error "illegal ellipsis in pattern"
 | ellipsisOccurences == 1 = if properLenght then (combineBindings [headMatches, ellipsisMatches, tailMatches]) else Nothing
 | otherwise = if (length patternList == paramLenght) then (zipAndCombineBindings (curriedMatch ellipsis literals) patternList paramList) else Nothing
  where
    properLenght = patternLenght <= paramLenght
    headMatches = if (headLenght == length headParams) then (zipAndCombineBindings (curriedMatch ellipsis literals) headPattern headParams) else Nothing
    ellipsisMatches = fmap (\patternList -> ellipsisTree ellipsisPattern patternList) ellipsisBindingTree
    ellipsisBindingTree = zipAndCombineEllipsisBindings (curriedMatch ellipsis literals) (repeat ellipsisPattern) ellipsisParams
    tailMatches = if (tailLenght == length tailParams) then (zipAndCombineBindings (curriedMatch ellipsis literals) tailPattern tailParams) else Nothing 
    headParams = take headLenght paramList
    tailParams = drop (paramLenght - tailLenght) paramList
    ellipsisParams = take ellipsisParamsLenght . drop headLenght $ paramList
    ellipsisParamsLenght = paramLenght - tailLenght - headLenght
    patternLenght = headLenght + tailLenght
    (headLenght, tailLenght) = mapTuple length (headPattern, tailPattern)
    (headPattern, ellipsisPattern) = (init tHeadPattern, last tHeadPattern)
    splitPattern@(tHeadPattern, tailPattern) = splitAtFirst ellipsisNode patternList
    patternList = makeList patternNode
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