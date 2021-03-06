module MacroEngine.MacroEngine where

import Parser.MetaNode
import qualified Data.Map as Map
import Data.List
import Data.Maybe (catMaybes, fromMaybe)
import MacroEngine.TemplateMetaNode
import MacroEngine.Base

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

-- flags for replacement algorithm
data ReplacementFlag = Found [Template] | Resolved [Template] | Outside deriving (Show)

-- *** binding constructs ***

makroengineBase :: String -> (MetaNode -> MetaNode)
makroengineBase name = case applySyntaxRules <$> Map.lookup name baseSyntax of
  Just val -> val
  Nothing -> error "base macro not defined"

makroengineDefine :: MetaNode -> (String, MetaNode -> MetaNode)
makroengineDefine = parseDefineSyntax
makroengineLet :: MetaNode -> [(String, MetaNode -> MetaNode)]
makroengineLet = parseLetSyntax parseBinding

makroengineLetIdentifiers :: MetaNode -> [MetaNode]
makroengineLetIdentifiers = parseLetSyntax getBindingIdentifier

parseDefineSyntax :: MetaNode -> (String, MetaNode -> MetaNode)
parseDefineSyntax (DefineSyntaxNode (IdentifierAtom name _) syntaxRules) = (name, applySyntaxRules syntaxRules)
parseDefineSyntax _ = error "wrong syntax for define-syntax"

parseLetSyntax :: (MetaNode -> a) -> MetaNode -> [a]
parseLetSyntax f (ApplicationNode rule1 rules) = map f (rule1:rules)
  -- rules is a 'transformer spec' in the standard

parseBinding :: MetaNode -> (String, MetaNode -> MetaNode)
parseBinding (ApplicationNode (IdentifierAtom name _) [rule]) = (name, applySyntaxRules rule)
parseBinding _ = error "Invalid syntax binding in let-syntax expression"

getBindingIdentifier :: MetaNode -> MetaNode
getBindingIdentifier (ApplicationNode ident@(IdentifierAtom _ _) [_]) = ident
getBindingIdentifier _ = error "Invalid syntax binding in let-syntax expression"

-- *** macro core ***

applySyntaxRules :: MetaNode -> MetaNode -> MetaNode
applySyntaxRules = apply . parseSyntaxRules

apply :: SyntaxRules -> MetaNode -> MetaNode
apply rules application = renderTemplate . transform bindingTree $ template
  where (bindingTree, template) = head . catMaybes . map ((\(SyntaxRule pattern template) -> (\b -> (b,template)) <$> matchApplication pattern application)) $ rules

parseSyntaxRules :: MetaNode -> SyntaxRules
-- syntax-rules without ellipsis argument
parseSyntaxRules (ApplicationNode (IdentifierAtom "syntax-rules" _) ((ApplicationNode literalsCar literalsCdr):syntaxRules)) =
  map (parseSyntaxRule "..." (map extractLiteral (literalsCar:literalsCdr))) syntaxRules
parseSyntaxRules (ApplicationNode (IdentifierAtom "syntax-rules" _) (EmptyAtom:syntaxRules)) =
  map (parseSyntaxRule "..." []) syntaxRules
-- syntax-rules with ellipsis argument
parseSyntaxRules (ApplicationNode (IdentifierAtom "syntax-rules" _) ((IdentifierAtom ellipsis _):(ApplicationNode literalsCar literalsCdr):syntaxRules)) =
  map (parseSyntaxRule ellipsis (map extractLiteral (literalsCar:literalsCdr))) syntaxRules
parseSyntaxRules (ApplicationNode (IdentifierAtom "syntax-rules" _) ((IdentifierAtom ellipsis _):EmptyAtom:syntaxRules)) =
  map (parseSyntaxRule ellipsis []) syntaxRules
parseSyntaxRules (ApplicationNode (IdentifierAtom "syntax-rules" _) _) = error "invalid syntax-rules syntax"
parseSyntaxRules _ = error "macro transformer via syntax-rules expected"

extractLiteral :: MetaNode -> Literal
extractLiteral (IdentifierAtom name _) = name
extractLiteral _ = error "invalid literal identifier given"

parseSyntaxRule :: Ellipsis -> [Literal] -> MetaNode -> SyntaxRule
parseSyntaxRule ellipsis literals (ApplicationNode patternNode [template]) =
  SyntaxRule Pattern{ellipsis=ellipsis, literals=literals, patternNode=patternNode} (analyseTemplate ellipsis [] template)
parseSyntaxRule _ _ _ = error "invalid syntax-rule syntax"

-- index -1 so that the indentifiers on root level do not yield Found replacement flags
transform :: BindingTree -> Template -> Template
transform bindingTree template = case transformM bindingTree 0 template of
  Just [result] -> result
  _ -> error "cannot transform template"

-- *** Transformation Algorithm ***

-- package this algorithm + calls to bindingLookup for regular identifier ellipsises
-- returns transformed Maybe [MetaNode] for transform elliptic to patch into parent list

-- nonErrorLookup of every identifier in subtemplate with the correct level (initial level +1 for subpattern ellipsis + any other identifier ellipses in template)
  -- only concat the lookup up to the level of the ellipsis itself
  -- see (syntax-rules () ((bloat2 ((name ...) (value ...)) ...) (list (list name ... ) ...)))
  -- for other subtemplate ellipsis
    -- check if reference to root level or context of current subpattern
    -- if yes then resolve with recursive call with level from root (only identifier ellipsis levels starting from zero)
    -- if no then resolve with recursive call with context level + further ellipsises in subtemplate
    -- to do this check just see if the respective recursive calls return Nothing, if both do, there is an error
-- if it is Just then it is a reference to a determining ellipsis -> append resulting [MetaNode] list to some "repeat list"
-- else try a errorLookup/bindingLookup without the ellipsis-level (only identifier ellipsis levels starting from zero)
  -- immediately apply transformations found
-- check
  -- all items of the "repeat list" must be of the same lenght
-- repeat the subtemplate for the lenght of the "repeat list" items and replace identifiers with the results from noErrorLookup

-- to identify the template to replace, just use the index of that element in the list


-- prerequisite of the algorithm is that the Template Datatype contains a unique path for every identifier in the template
-- it then builds a map construct of all paths and their respective identifier nodes
-- with the function nonErrorLookupLabel this map is expanded into a replacement table, so it is now a list of 3-tuples carrying an additional
-- replacement flag. This flag can be Resolved for fully transformed MetaNode results (like root-level references),
-- Outside for outside references and Found. Found is a special flag that symbolises the need for replication in subtemplate ellipsises
-- After building the replacement table, the replace function applies the replacement table onto the given raw template. In doing so
-- it guarantees that all Replacements with the flag Found have the same lenght and that subtemplates are replicated as needed with
-- the index of the element taken from the Found flag increasing by one for each iteration.

-- transformM combines the building of a replacement table and the actual replacement process
-- transform calls the generic transformM with ellipsis level 0 for root level templates

transformM :: BindingTree -> Integer -> Template -> Maybe [Template]
transformM bindingTree level template = (\rt->replace rt template) <$> replicateReplacementTable <$> nonErrorLookupLabels level bindingTree identifierTable
  where identifierTable = identifierLabelTable template

replace :: [[([Integer], Template, ReplacementFlag)]] -> Template -> [Template]
replace tables template = map (\table -> replaceSingle table template) tables

replaceSingle :: [([Integer], Template, ReplacementFlag)] -> Template -> Template
replaceSingle table template = replace' template
  where
    replace' (TemplateListNode xs) = TemplateListNode (replaceList xs)
    replace' (TemplateImproperListNode xs) = TemplateImproperListNode (replaceList xs)
    replace' (TemplateIdentifierAtom path _ _)
      | length replacement == 1 = head replacement
      | otherwise = error "multiple values to replace for non-elliptic identifier"
      where
        replacement = lookup path
    replace' (TemplateLambdaNode a b c) = TemplateLambdaNode (replaceList a) (replace' b) (replaceList c)
    replace' (TemplateIfNode a b c) = TemplateIfNode (replace' a) (replace' b) (replace' c)
    replace' (TemplateLetSyntaxNode a b) = TemplateLetSyntaxNode (replace' a) (replaceList b)
    replace' (TemplateLetrecSyntaxNode a b) = TemplateLetrecSyntaxNode (replace' a) (replaceList b)
    replace' (TemplateDefineSyntaxNode a b) = TemplateDefineSyntaxNode (replace' a) (replace' b)
    replace' o = o
    replaceList ((TemplateIdentifierAtom path _ _) :xs) = ( lookup path ++ replaceList xs)
    replaceList  ((TemplateEllipsisNode _ path _):xs) = ( lookup path ++ replaceList xs )
    replaceList  (x:xs) = ( replace' x : replaceList xs)
    replaceList  [] = []
    lookup path = case find (\(p, template, replacement) -> p == path) table of
      Just (_, _, Resolved nl) -> nl
      Just (_, ident, Outside) -> [ident]
      Just (_, _, replacement) -> error ("forbidden replacement flag " ++ show replacement ++ " found in replace stage")
      Nothing -> error "path without replacement"

replicateReplacementTable :: [([Integer], Template, ReplacementFlag)] -> [[([Integer], Template, ReplacementFlag)]]
replicateReplacementTable table = if length founds == 0 
  -- no replication, just one occurence
  then [table] 
  else zipWith (\t index -> map (replaceFound index) t) (replicate foundLength table) [0..]
    where
      replaceFound i (path, t, (Found xs)) = (path, t, (Resolved [(xs !! i)]))
      replaceFound _ r = r
      foundLength = if allTheSame foundLengths then head foundLengths else error "input list lenghts does not match"
      foundLengths = map length . map foundList $ founds
      founds = filter isFound table
      isFound (_, _, (Found _)) = True
      isFound _ = False 
      foundList (_, _, (Found xs)) = xs
      allTheSame xs = and $ map (== head xs) (tail xs)

-- for semantic analysis
incrementIdentifiers :: ([Integer], Template, ReplacementFlag) -> ([Integer], Template, ReplacementFlag)
incrementIdentifiers (path, template, Resolved replacement) = (path, template, Resolved (map increment replacement))
incrementIdentifiers (path, template, Found replacement) = (path, template, Found (map increment replacement))
incrementIdentifiers rte@(_, _, Outside) = rte

increment :: Template -> Template
increment (TemplateIdentifierAtom path str i) = TemplateIdentifierAtom path str (i + 1)
increment (TemplateListNode a) = TemplateListNode (map increment a)
increment (TemplateImproperListNode a) = TemplateImproperListNode (map increment a)
increment (TemplateIfNode a b c) = TemplateIfNode (increment a) (increment b) (increment c)
increment (TemplateSetNode a b) = TemplateSetNode (increment a) (increment b)
increment (TemplateDefineNode a b) = TemplateDefineNode (increment a) (increment b)
increment (TemplateLetSyntaxNode a b) = TemplateLetSyntaxNode (increment a) (map increment b)
increment (TemplateDefineSyntaxNode a b) = TemplateDefineSyntaxNode (increment a) (increment b)
increment (TemplateLetrecSyntaxNode a b) = TemplateLetrecSyntaxNode (increment a) (map increment b)
increment (TemplateAtom a) = TemplateAtom (incrementMN a)
increment other = other

incrementMN :: MetaNode -> MetaNode
incrementMN (IdentifierAtom str i) = IdentifierAtom str (i + 1)
incrementMN (LambdaNode a b c) = LambdaNode (map incrementMN a) (incrementMN b) (map incrementMN c)
incrementMN (PairNode a b) = PairNode (incrementMN a) (incrementMN b)
incrementMN (ApplicationNode a b) = ApplicationNode (incrementMN a) (map incrementMN b)
incrementMN (IfNode a b c) = IfNode (incrementMN a) (incrementMN b) (incrementMN c)
incrementMN (SetNode a b) = SetNode (incrementMN a) (incrementMN b)
incrementMN (DefineNode a b) = DefineNode (incrementMN a) (incrementMN b)
incrementMN (LetSyntaxNode a b) = LetSyntaxNode (incrementMN a) (map incrementMN b)
incrementMN (DefineSyntaxNode a b) = DefineSyntaxNode (incrementMN a) (incrementMN b)
incrementMN (LetrecSyntaxNode a b) = LetrecSyntaxNode (incrementMN a) (map incrementMN b)
incrementMN other = other

-- resolving nested ellipsis
-- only concat to level and supply an index to lookup

nonErrorLookupLabels :: Integer -> BindingTree -> [([Integer], Template)] -> Maybe [([Integer], Template, ReplacementFlag)]
nonErrorLookupLabels level bindingTree identifierTable = sequence . map lookup $ identifierTable
  where patternVariables = nub $ bindingTree >>= getPatternVariables
        lookup (path, i@(TemplateIdentifierAtom _ str _)) 
          | level == 0 = lookupIdentifier Resolved path i level str
          | otherwise = lookupIdentifier Found path i level str
        -- different from normal identifier lookup in that everything should be resolved and not found
        -- TODO may clash with ongoing fixes of nested subtemplate ellipses
        lookup (path, e@(TemplateEllipsisNode count _ i@(TemplateIdentifierAtom _ str _)))
          | level == 0 = lookupIdentifier Resolved path e count str
          | otherwise = lookupIdentifier Found path e (level + count) str
        lookup (path, e@(TemplateEllipsisNode count _ node)) = resolveEllipsis
          where
            resolveEllipsis = case transformM bindingTree count node of
              Just resolved -> Just (path, e, Resolved resolved)
              -- not root level
              -- do not fully concat 
              Nothing -> case transformM bindingTree (count + level) node of
                Just resolved -> Just (path, e, Resolved resolved)
                Nothing -> Nothing
        lookupIdentifier f path identifier iLevel str
          | str `elem` patternVariables = case nonErrorLookup iLevel str bindingTree of
              Nothing -> case nonErrorLookup 0 str bindingTree of
                Just replacement -> Just (incrementIdentifiers (path, identifier, Resolved (map TemplateAtom replacement)))
                Nothing -> Nothing
              Just replacement -> Just (incrementIdentifiers (path, identifier, f (map TemplateAtom replacement)))
          | otherwise = Just (path, identifier, Outside)

getPatternVariables :: Binding -> [String]
getPatternVariables (SubPatternEllipsis _ xs) = xs >>= (\bindingTree -> bindingTree >>= getPatternVariables)
getPatternVariables (IdentifierEllipsis (IdentifierAtom str _) _) = [str]
getPatternVariables (Value (IdentifierAtom str _) _) = [str]
getPatternVariables (Empty) = []

identifierLabelTable :: Template -> [([Integer], Template)]
identifierLabelTable (TemplateListNode xs) = xs >>= identifierLabelTable
identifierLabelTable (TemplateImproperListNode xs) = xs >>= identifierLabelTable
identifierLabelTable (TemplateLambdaNode a b c) = (a >>= identifierLabelTable) ++ identifierLabelTable b ++ (c >>= identifierLabelTable)
identifierLabelTable (TemplateIfNode a b c) = identifierLabelTable a ++ identifierLabelTable b ++ identifierLabelTable c
identifierLabelTable (TemplateDefineNode a b) = identifierLabelTable a ++ identifierLabelTable b
identifierLabelTable (TemplateSetNode a b) = identifierLabelTable a ++ identifierLabelTable b
identifierLabelTable (TemplateLetSyntaxNode a b) = identifierLabelTable a ++ (b >>= identifierLabelTable)
identifierLabelTable (TemplateLetrecSyntaxNode a b) = identifierLabelTable a ++ (b >>= identifierLabelTable)
identifierLabelTable (TemplateDefineSyntaxNode a b) = identifierLabelTable a ++ identifierLabelTable b
identifierLabelTable ident@(TemplateIdentifierAtom path _ _) = [(path, ident)]
identifierLabelTable ellipsis@(TemplateEllipsisNode count path _) = [(path, ellipsis)]
identifierLabelTable _ = []

-- Lookups in BindingTree

nonErrorLookup :: Integer -> String -> BindingTree -> Maybe [MetaNode]
nonErrorLookup = genericBindingLookup (\_ -> Nothing)

bindingLookup :: Integer -> String -> BindingTree -> Maybe [MetaNode]
bindingLookup = genericBindingLookup error

genericBindingLookup :: (String -> Maybe [MetaNode]) -> Integer -> String -> BindingTree -> Maybe [MetaNode]
genericBindingLookup f 0 identifier bindingTree
  | Just (Value _ val) <- bindingOnSameLevel = Just [val]
  | Nothing <- bindingOnSameLevel = Nothing
  | otherwise = f "too many ellipses"
  where bindingOnSameLevel = searchIdentifier identifier bindingTree
genericBindingLookup f 1 identifier bindingTree
  | Just (IdentifierEllipsis _ values) <- bindingOnSameLevel = Just values
  where bindingOnSameLevel = searchIdentifier identifier bindingTree
genericBindingLookup f remainingLevel identifier bindingTree
  | Just (SubPatternEllipsis _ subPatterns) <- bindingOnSameLevel = concat <$> sequence (map (genericBindingLookup f (remainingLevel - 1) identifier) subPatterns)
  | Nothing <- bindingOnSameLevel = Nothing
  | otherwise = f "not enough ellipses"
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
containsIdentifier str (IdentifierAtom i _) = str == i
containsIdentifier str p@(PairNode _ _) = or . map (containsIdentifier str) $ makeList p
containsIdentifier str a@(ApplicationNode _ _) = or . map (containsIdentifier str) $ makeList a
-- todo if, define, set
containsIdentifier _ _ = False

-- *** MetaNode/Template Conversion ***

renderTemplate :: Template -> MetaNode
renderTemplate (TemplateListNode list) = ApplicationNode (renderTemplate (head list)) (map renderTemplate (tail list))
renderTemplate (TemplateImproperListNode [a,b]) = PairNode (renderTemplate a) (renderTemplate b)
renderTemplate (TemplateImproperListNode list) = PairNode (renderTemplate (head list)) (renderTemplate (TemplateImproperListNode (tail list)))
renderTemplate (TemplateAtom atom) = atom
renderTemplate (TemplateLambdaNode a b c) = LambdaNode (map renderTemplate a) (renderTemplate b) (map renderTemplate c)
renderTemplate (TemplateIdentifierAtom _ identifierStr num)  = IdentifierAtom identifierStr num
renderTemplate (TemplateSetNode a b) = SetNode (renderTemplate a) (renderTemplate b)
renderTemplate (TemplateDefineNode a b) = DefineNode (renderTemplate a) (renderTemplate b)
renderTemplate (TemplateIfNode a b c) = IfNode (renderTemplate a) (renderTemplate b) (renderTemplate c)
renderTemplate (TemplateLetSyntaxNode a b) = LetSyntaxNode (renderTemplate a) (map renderTemplate b)
renderTemplate (TemplateLetrecSyntaxNode a b) = LetrecSyntaxNode (renderTemplate a) (map renderTemplate b)
renderTemplate (TemplateDefineSyntaxNode a b) = DefineSyntaxNode (renderTemplate a) (renderTemplate b)


analyseTemplate :: Ellipsis -> [Integer] -> MetaNode -> Template
analyseTemplate ellipsis path a@(ApplicationNode _ _) = TemplateListNode (analyseTemplateList ellipsis path (makeList a))
analyseTemplate ellipsis path p@(PairNode _ _) = TemplateImproperListNode (analyseTemplateList ellipsis path (makeList p))
-- TODO fix path
analyseTemplate ellipsis path (LambdaNode a b c) = TemplateLambdaNode (analyseTemplateList ellipsis (path ++ [2]) a) (analyseTemplate ellipsis (path ++ [1]) b) (analyseTemplateList ellipsis (path ++ [0]) c)
analyseTemplate ellipsis path (IfNode a b c) = TemplateIfNode (analyseTemplate ellipsis (path ++ [2]) a) (analyseTemplate ellipsis (path ++ [1]) b) (analyseTemplate ellipsis (path ++ [0]) c)
analyseTemplate _ _ atom@(NumberAtom a) = TemplateAtom atom
analyseTemplate _ _ atom@(EmptyAtom) = TemplateAtom atom
analyseTemplate _ _ atom@(StringAtom a) = TemplateAtom atom
analyseTemplate _ _ atom@(BoolAtom a) = TemplateAtom atom
analyseTemplate _ _ atom@(CharAtom a) = TemplateAtom atom
analyseTemplate _ path (IdentifierAtom a num) = TemplateIdentifierAtom path a num
analyseTemplate ellipsis path (SetNode a b) = TemplateSetNode (analyseTemplate ellipsis (path ++ [1]) a) (analyseTemplate ellipsis (path ++ [0]) b)
analyseTemplate ellipsis path (LetSyntaxNode a b) = TemplateLetSyntaxNode (analyseTemplate ellipsis (path ++ [1]) a) (analyseTemplateList ellipsis (path ++ [0]) b)
analyseTemplate ellipsis path (LetrecSyntaxNode a b) = TemplateLetrecSyntaxNode (analyseTemplate ellipsis (path ++ [1]) a) (analyseTemplateList ellipsis (path ++ [0]) b)
analyseTemplate ellipsis path (DefineSyntaxNode a b) = TemplateDefineSyntaxNode (analyseTemplate ellipsis (path ++ [1]) a) (analyseTemplate ellipsis (path ++ [0]) b)
analyseTemplate ellipsis path (DefineNode a b) = TemplateDefineNode (analyseTemplate ellipsis (path ++ [1]) a) (analyseTemplate ellipsis (path ++ [0]) b)

analyseTemplateList :: Ellipsis -> [Integer] ->[MetaNode] -> [Template]
analyseTemplateList ellipsis path nodeList = (\(a, _, _) -> a) . foldr (wrapOnEllipsis ellipsis path) ([],0, 0) $ nodeList

wrapOnEllipsis :: Ellipsis -> [Integer] -> MetaNode -> ([Template], Integer, Integer) -> ([Template], Integer, Integer)
wrapOnEllipsis ellipsis path metaNode (nl, wrapCount, index)
  | isIdentifierAndEqual ellipsis metaNode = (nl, wrapCount + 1, index)
  | otherwise = ((wrap metaNode wrapCount):nl, 0, index + 1)
  where
    wrap node 0 = analyseTemplate ellipsis (path ++ [index]) node
    wrap node count = TemplateEllipsisNode count (path ++ [index]) (analyseTemplate ellipsis (path ++ [index,0]) node)
    isIdentifierAndEqual str (IdentifierAtom istr _) = str == istr
    isIdentifierAndEqual _ _ = False

-- *** Pattern Matching ***

-- verify if macro call matches a pattern
matchApplication :: Pattern -> MetaNode -> Maybe BindingTree
-- pattern has to be in scheme form for matchList to operate recursively
-- it can be an application node or a pair node (top-level pattern has to be of list form)
matchApplication pattern@Pattern{patternNode=patternNode} params@(ApplicationNode _ _)
  | (ApplicationNode (IdentifierAtom _ _) xs) <- patternNode = matchList pattern{patternNode = (ApplicationNode (IdentifierAtom "_" 0) xs)} params
  | (PairNode (IdentifierAtom _ _) cdr) <- patternNode = matchList pattern{patternNode = (PairNode (IdentifierAtom "_" 0) cdr)} params
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
match Pattern{patternNode = (IdentifierAtom "_" _)} _ = Just emptyTree
-- if p is non literal
match Pattern{literals = literals, patternNode = p@(IdentifierAtom pidentifier _)} e@(IdentifierAtom identifier _)
-- TODO discuss role of "lexical binding" in r7rs page 23, right column
  | pidentifier `elem` literals && identifier /= pidentifier = Nothing
  | otherwise = Just $ singletonTree p e

match Pattern{literals=literals, patternNode = p@(IdentifierAtom pidentifier _)} e
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
    splitPattern@(tHeadPattern, tailPattern) = splitAtFirstIdentifier ellipsis patternList
    patternList = makeList patternNode
    paramLenght = length paramList
    paramList = makeList params
    ellipsisOccurences = countIdentifiers ellipsis patternList
    

makeList :: MetaNode -> [MetaNode]
makeList (ApplicationNode x xs) = x : xs
makeList (PairNode car cdr) = car : (makeList cdr)
-- handle proper and improper lists the same way
makeList (EmptyAtom) = []
makeList node = [node]

-- split list at ellipsis
splitAtFirst :: Eq a => a -> [a] -> ([a], [a])
splitAtFirst x = fmap (drop 1) . break (x ==)

splitAtFirstIdentifier :: String -> [MetaNode] -> ([MetaNode], [MetaNode])
splitAtFirstIdentifier x = fmap (drop 1) . break breakFunc
  where
    breakFunc (IdentifierAtom str _) = x == str
    breakFunc _ = False

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a1, a2) = (f a1, f a2)

count   :: Eq a => a -> [a] -> Int
count x =  length . filter (==x)

countIdentifiers :: String -> [MetaNode] -> Int
countIdentifiers str = count str . map (\(IdentifierAtom str _) -> str) . filter isIdentifier

isIdentifier :: MetaNode -> Bool
isIdentifier (IdentifierAtom _ _) = True
isIdentifier _ = False

toFst :: (a -> b) -> a -> (b, a)
toFst f a = (f a, a)