module MacroEngine.MacroEngine where

import Parser.MetaNode
import qualified Data.Map as Map
import Data.List
import Data.Maybe (catMaybes, fromMaybe)
import MacroEngine.TemplateMetaNode

type PatternNode = MetaNode
type Literal = String
type Ellipsis = String

-- tree like structure for bindings
type BindingTree = [Binding]
data Binding = SubPatternEllipsis MetaNode [BindingTree] | IdentifierEllipsis MetaNode [MetaNode] | Value MetaNode MetaNode | Empty deriving (Show, Eq)

-- structure for patterns and templates
data Pattern = Pattern { ellipsis :: Ellipsis, literals :: [Literal], patternNode :: PatternNode}
type Template = TemplateMetaNode
-- data Template = Level MetaNode | TemplateEllipsis

data SyntaxRule = SyntaxRule Pattern Template
type SyntaxRules = [SyntaxRule]

type Transformer = MetaNode -> MetaNode

applySyntaxRules :: MetaNode -> MetaNode -> MetaNode
applySyntaxRules = apply . parseSyntaxRules

apply :: SyntaxRules -> MetaNode -> MetaNode
apply rules application = renderTemplate . transform bindingTree $ template
  where (bindingTree, template) = head . catMaybes . map ((\(SyntaxRule pattern template) -> (\b -> (b,template)) <$> matchApplication pattern application)) $ rules

renderTemplate :: Template -> MetaNode
renderTemplate (TemplateListNode list) = ApplicationNode (renderTemplate (head list)) (map renderTemplate (tail list))
renderTemplate (TemplateImproperListNode [a,b]) = PairNode (renderTemplate a) (renderTemplate b)
renderTemplate (TemplateImproperListNode list) = PairNode (renderTemplate (head list)) (renderTemplate (TemplateImproperListNode (tail list)))
renderTemplate (TemplateAtom atom) = atom
renderTemplate (TemplateLambdaNode a b c) = LambdaNode (map renderTemplate a) (renderTemplate b) (map renderTemplate c)
renderTemplate (TemplateIdentifierAtom identifierStr) = IdentifierAtom identifierStr
renderTemplate (TemplateSetNode a b) = SetNode (renderTemplate a) (renderTemplate b)
renderTemplate (TemplateDefineNode a b) = DefineNode (renderTemplate a) (renderTemplate b)

transform :: BindingTree -> Template -> Template
-- transform :: bindings -> template -> transformed template
transform = transformElliptic 0

transformElliptic :: Integer -> BindingTree -> Template -> Template
--                                                                                                                                          |<- fix this shit, just here to compile
-- this need context from the parent
transformElliptic level bindingTree identifier@(TemplateIdentifierAtom identifierString) = fromMaybe identifier (ellipsisFreeTemplate <$> head <$> bindingLookup level identifierString bindingTree) 
transformElliptic level bindingTree (TemplateEllipsisNode subTemplate) = transformElliptic (level+1) bindingTree subTemplate 
--transformElliptic level bindingTree (TemplateEllipsisNode subTemplate@(TemplateIdentifierAtom _)) = transformElliptic (level+1) bindingTree subTemplate 
--transformElliptic level bindingTree (TemplateEllipsisNode subTemplate@(TemplateEllipsisNode _)) = transformElliptic (level+1) bindingTree subTemplate 
--transformElliptic level bindingTree (TemplateEllipsisNode subTemplate@(TemplateIdentifierAtom _)) = transformElliptic (level+1) bindingTree subTemplate 
-- complex elliptic subpattern
-- adjust levels
-- search unique ellipsis subtree (normal case: identifiers in subtemplate are either free references or refer to the same subpatternellipsis
-- a special case is that pattern variables refer to different subpatternellipses, where the lenght of input parameters has to match)
-- identifiers and ellipses in the subtemplate can still refer to level 0
--transformElliptic level bindingTree (TemplateEllipsisNode subTemplate@(TemplateListNode _)) = transformElliptic (level+1) bindingTree subTemplate 
-- other template ellipsis nodes like numbers etc are invalid
transformElliptic level bindingTree (TemplateListNode cdr) = TemplateListNode (map (transformElliptic level bindingTree) cdr)
transformElliptic level bindingTree (TemplateImproperListNode cdr) = TemplateImproperListNode (map (transformElliptic level bindingTree) cdr)
-- todo <ellipsis> <template>
transformElliptic _ _ atom = atom

levelLookup :: String -> BindingTree -> Maybe Template
levelLookup e bindings = ellipsisFreeTemplate <$> (\(Value _ k) -> k) <$> find isSameIdentifier bindings
  where isSameIdentifier (Value (IdentifierAtom k) v) = k == e
        isSameIdentifier _ = False



bindingLookup :: Integer -> String -> BindingTree -> Maybe [MetaNode]
bindingLookup 0 identifier bindingTree
  | Just (Value _ val) <- bindingOnSameLevel = Just [val]
  | Nothing <- bindingOnSameLevel = Nothing
  | otherwise = error "too many ellipses"
  where bindingOnSameLevel = searchIdentifier identifier bindingTree
bindingLookup 1 identifier bindingTree
  | Just (IdentifierEllipsis _ values) <- bindingOnSameLevel = Just values
  where bindingOnSameLevel = searchIdentifier identifier bindingTree
bindingLookup remainingLevel identifier bindingTree
  | Just (SubPatternEllipsis _ subPatterns) <- bindingOnSameLevel = concat <$> sequence (map (bindingLookup (remainingLevel - 1) identifier) subPatterns)
  | Nothing <- bindingOnSameLevel = Nothing
  | otherwise = error "not enough ellipses"
  where bindingOnSameLevel = searchIdentifier identifier bindingTree

searchIdentifier :: String -> BindingTree -> Maybe Binding
searchIdentifier identifier bindingList
  | [binding] <- results = Just binding
  | (x:xs) <- results = error "ambigous pattern variable"
  | otherwise = Nothing
  where results = filter (hasIdentifier identifier) bindingList

hasIdentifier :: String -> Binding -> Bool
hasIdentifier str Empty = False
hasIdentifier str (Value k v) = containsIdentifier str k
hasIdentifier str (SubPatternEllipsis n _) = containsIdentifier str n
hasIdentifier str (IdentifierEllipsis n _) = containsIdentifier str n

containsIdentifier :: String -> MetaNode -> Bool
containsIdentifier str (IdentifierAtom i) = str == i
containsIdentifier str p@(PairNode _ _) = or . map (containsIdentifier str) $ makeList p
containsIdentifier str a@(ApplicationNode _ _) = or . map (containsIdentifier str) $ makeList a
-- todo if, define, set
containsIdentifier _ _ = False

parseSyntaxRules :: MetaNode -> SyntaxRules
-- syntax-rules without ellipsis argument
parseSyntaxRules (ApplicationNode (IdentifierAtom "syntax-rules") ((ApplicationNode literalsCar literalsCdr):syntaxRules)) =
  map (parseSyntaxRule "..." (map extractLiteral (literalsCar:literalsCdr))) syntaxRules
parseSyntaxRules (ApplicationNode (IdentifierAtom "syntax-rules") (EmptyAtom:syntaxRules)) =
  map (parseSyntaxRule "..." []) syntaxRules
-- syntax-rules with ellipsis argument
parseSyntaxRules (ApplicationNode (IdentifierAtom "syntax-rules") ((IdentifierAtom ellipsis):(ApplicationNode literalsCar literalsCdr):syntaxRules)) =
  map (parseSyntaxRule ellipsis (map extractLiteral (literalsCar:literalsCdr))) syntaxRules
parseSyntaxRules (ApplicationNode (IdentifierAtom "syntax-rules") ((IdentifierAtom ellipsis):EmptyAtom:syntaxRules)) =
  map (parseSyntaxRule ellipsis []) syntaxRules
parseSyntaxRules (ApplicationNode (IdentifierAtom "syntax-rules") _) = error "invalid syntax-rules syntax"
parseSyntaxRules _ = error "macro transformer via syntax-rules expected"

extractLiteral :: MetaNode -> Literal
extractLiteral (IdentifierAtom name) = name
extractLiteral _ = error "invalid literal identifier given"

parseSyntaxRule :: Ellipsis -> [Literal] -> MetaNode -> SyntaxRule
parseSyntaxRule ellipsis literals (ApplicationNode patternNode [template]) =
  SyntaxRule Pattern{ellipsis=ellipsis, literals=literals, patternNode=patternNode} (analyseTemplate ellipsis template)
parseSyntaxRule _ _ _ = error "invalid syntax-rule syntax"

analyseTemplate :: Ellipsis -> MetaNode -> Template
analyseTemplate ellipsis a@(ApplicationNode _ _) = TemplateListNode (analyseTemplateList ellipsis (makeList a))
analyseTemplate ellipsis p@(PairNode _ _) = TemplateImproperListNode (analyseTemplateList ellipsis (makeList p))
analyseTemplate ellipsis (LambdaNode a b c) = TemplateLambdaNode (map (analyseTemplate ellipsis) a) (analyseTemplate ellipsis b) (map (analyseTemplate ellipsis) c)
analyseTemplate _ atom@(NumberAtom a) = TemplateAtom atom
analyseTemplate _ atom@(EmptyAtom) = TemplateAtom atom
analyseTemplate _ atom@(StringAtom a) = TemplateAtom atom
analyseTemplate _ atom@(BoolAtom a) = TemplateAtom atom
analyseTemplate _ atom@(CharAtom a) = TemplateAtom atom
analyseTemplate _ (IdentifierAtom a) = TemplateIdentifierAtom a
analyseTemplate ellipsis (SetNode a b) = TemplateSetNode (analyseTemplate ellipsis a) (analyseTemplate ellipsis b)
analyseTemplate ellipsis (DefineNode a b) = TemplateDefineNode (analyseTemplate ellipsis a) (analyseTemplate ellipsis b)

ellipsisFreeTemplate :: MetaNode -> Template
ellipsisFreeTemplate a@(ApplicationNode _ _) = TemplateListNode (map ellipsisFreeTemplate (makeList a))
ellipsisFreeTemplate p@(PairNode _ _) = TemplateImproperListNode (map ellipsisFreeTemplate (makeList p))
ellipsisFreeTemplate (LambdaNode a b c) = TemplateLambdaNode (map ellipsisFreeTemplate a) (ellipsisFreeTemplate b) (map ellipsisFreeTemplate c)
ellipsisFreeTemplate atom@(NumberAtom a) = TemplateAtom atom
ellipsisFreeTemplate atom@(EmptyAtom) = TemplateAtom atom
ellipsisFreeTemplate atom@(StringAtom a) = TemplateAtom atom
ellipsisFreeTemplate atom@(BoolAtom a) = TemplateAtom atom
ellipsisFreeTemplate atom@(CharAtom a) = TemplateAtom atom
ellipsisFreeTemplate (IdentifierAtom a) = TemplateIdentifierAtom a
ellipsisFreeTemplate (SetNode a b) = TemplateSetNode (ellipsisFreeTemplate a) (ellipsisFreeTemplate b)
ellipsisFreeTemplate (DefineNode a b) = TemplateDefineNode (ellipsisFreeTemplate a) (ellipsisFreeTemplate b)

analyseTemplateList :: Ellipsis -> [MetaNode] -> [Template]
analyseTemplateList ellipsis = fst . foldr (wrapOnEllipsis ellipsis) ([],0)

wrapOnEllipsis :: Ellipsis -> MetaNode -> ([Template], Integer) -> ([Template], Integer)
wrapOnEllipsis ellipsis metaNode (nl, wrapCount)
  | metaNode == (IdentifierAtom ellipsis) = (nl, wrapCount + 1)
  | otherwise = ((wrap metaNode wrapCount):nl,0)
  where
    wrap node 0 = analyseTemplate ellipsis node
    wrap node count = TemplateEllipsisNode (wrap node (count - 1))

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

zipAndCombineEllipsisBindings :: (PatternNode -> MetaNode -> Maybe BindingTree) -> [PatternNode] -> [MetaNode] -> Maybe [BindingTree]
zipAndCombineEllipsisBindings f pl el = sequence . zipWith f pl $ el

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

ellipsisBinding :: MetaNode -> [[Binding]] -> Binding
ellipsisBinding p@(PairNode _ _) bindings = SubPatternEllipsis p bindings
ellipsisBinding p@(ApplicationNode _ _) bindings = SubPatternEllipsis p bindings
ellipsisBinding pattern bindings = IdentifierEllipsis pattern (map singletonTreeToEllipsisValue (filter isNotSingletonEmpty (bindings)))
  where
    singletonTreeToEllipsisValue [(Value _ v)] = v
    -- exhaustion is error because constant ellipsis patterns are already filtered
    -- IdentifierEllipsis cannot have another Ellipsis inside
    isNotSingletonEmpty [Empty] = False
    isNotSingletonEmpty _ = True

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
    ellipsisMatches = fmap (\bindingList -> [ellipsisBinding ellipsisPattern bindingList]) ellipsisBindingTree
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

toFst :: (a -> b) -> a -> (b, a)
toFst f a = (f a, a)