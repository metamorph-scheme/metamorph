module SemanticAnalysis.SemanticAnalysis (
    semanticAnalysis
) where
import Control.Monad.State.Lazy
import Common.Number
import SemanticAnalysis.MetaNode'
import Parser.MetaNode
import MacroEngine.MacroEngine
type AnalysisState = SymbolTable

--dbgPrint n mn = trace ("\n Debug " ++ show n ++ ": " ++ show mn ++ "\n")

pushActivationEntries :: [String] -> State AnalysisState ()
pushActivationEntries params = do
    let newentries = zip (filter (not . null) params) [0..]
    oldentries <- get
    put (Activation newentries oldentries)

pushSyntaxEntries :: [(String, MetaNode->MetaNode)] -> State AnalysisState ()
pushSyntaxEntries newentries = do
    oldentries <- get
    put (Syntax newentries oldentries)

pushScope :: Int -> State AnalysisState ()
pushScope n = do
    entries <- get
    put (Scope n entries)

popEntries :: State AnalysisState ()
popEntries = do
    entries <- get
    case entries of
        Activation _ table -> put table
        Syntax _ table -> put table
        Scope _ table -> put table
        Global -> error "Internal Compiler Error: popEntries on empty symbol table"

pushDefines :: [MetaNode] -> State AnalysisState [MetaNode]
pushDefines ((DefineNode (IdentifierAtom str _) trg):ms) = do
    oldentries <- get
    case oldentries of
        Activation entries next -> do
            put $ Activation ((str, length entries):entries) next
            ms' <- pushDefines (injectName (IdentifierAtom str 0) <$> ms)
            return $ SetNode (IdentifierAtom str 0) trg:ms'
        _ -> error "Internal Compiler Error: Incorrect Usage of pushDefines"
pushDefines ((DefineNode _ _):ms) = error "Expected unbound identifier as first argument of define"
pushDefines (m:ms) = do
    ms' <- pushDefines ms
    return (m:ms')
pushDefines [] = return []

pushDefineSyntax :: [MetaNode] -> State AnalysisState [MetaNode]
pushDefineSyntax (definesyntax@(DefineSyntaxNode (IdentifierAtom str _) _):ms) = do
    oldentries <- get
    case oldentries of
        Syntax entries next -> do
            put $ Syntax  (makroengineDefine definesyntax:entries) next
            pushDefineSyntax (injectName (IdentifierAtom str 0) <$> ms)
        _ -> error "Internal Compiler Error: Incorrect Usage of pushDefineSyntax"
pushDefineSyntax ((DefineSyntaxNode _ _):ms) =  error "Expected unbound identifier as first argument of define-syntax"
pushDefineSyntax (m:ms) = do
    ms' <- pushDefineSyntax ms
    return (m:ms')
pushDefineSyntax [] = return  []

semanticAnalysis :: [MetaNode] -> MetaNode'
semanticAnalysis mn =
    evalState (annotateBody mn False) Global

annotateBody :: [MetaNode] -> Bool -> State AnalysisState MetaNode'
annotateBody mn tailpos = do
    pushActivationEntries [] -- Activation entries need to be lower in the stack then syntax entries
    mn' <- pushDefines mn
    tb <- get
    case tb of
        -- dont count empty bodies
        (Activation [] _) -> do
            popEntries -- bound numbers will not include empty body activation
            pushSyntaxEntries [] -- makro scopes will include beginning at syntax entries
            mn'' <- pushDefineSyntax mn'
            mn''' <- annotateList mn'' tailpos
            popEntries -- SyntaxEntries
            return $ BodyNode' 0 mn'''
        (Activation entries _) -> do
            pushSyntaxEntries [] -- makro scopes will include beginning at syntax entries
            mn'' <- pushDefineSyntax mn'
            mn''' <- annotateList mn'' tailpos
            popEntries -- Syntax Entries
            popEntries -- Define Entries
            return $ BodyNode' (length entries) mn'''
        _ -> error "Internal Compiler Error: Faulty symbol table"

annotateList :: [MetaNode] -> Bool -> State AnalysisState [MetaNode']
annotateList [] _ = return []
annotateList [e] tailpos = do
    e' <- annotateExpression e tailpos
    return [e']
annotateList (e:es) tailpos = do
    e' <- annotateExpression e False
    es' <- annotateList es tailpos
    return $ e':es'

annotateExpression :: MetaNode -> Bool -> State AnalysisState MetaNode'
annotateExpression lambda@(LambdaNode _ _ _) _ = annotateLambda lambda
annotateExpression application@(ApplicationNode _ _) tailpos = annotateApplication application tailpos
annotateExpression identifier@(IdentifierAtom _ _) _ = annotateIdentifier identifier
annotateExpression letsyntax@(LetSyntaxNode _ _ ) tailpos = annotateLetSyntax letsyntax tailpos
annotateExpression letrecsyntax@(LetrecSyntaxNode _ _) tailpos = annotateLetrecSyntax letrecsyntax tailpos
-- Recursive Trivial Annotations
annotateExpression (PairNode car cdr) _ = do
    car' <- annotateExpression car False
    cdr' <- annotateExpression cdr False
    return $ PairNode' car' cdr'
annotateExpression (IfNode ifexpr thenbr elsebr) tailpos = do
    ifexpr' <- annotateExpression ifexpr False
    thenbr' <- annotateExpression thenbr tailpos
    elsebr' <- annotateExpression elsebr tailpos
    return $ IfNode' ifexpr' thenbr' elsebr'
annotateExpression (SetNode src@(IdentifierAtom _ _) trg) _ = do
    src' <- annotateExpression src False
    trg' <- annotateExpression trg False
    return $ SetNode' src' trg'
annotateExpression (SetNode _ _) _ = error "Expected identifier as first argument of set!"
-- Trivial Annotations
annotateExpression (NumberAtom n) _ = return $ NumberAtom' n
annotateExpression EmptyAtom _ = return $ EmptyAtom'
annotateExpression (StringAtom cs) _ = return $ StringAtom' cs
annotateExpression (BoolAtom b) _ = return $ BoolAtom' b
annotateExpression (CharAtom c) _ = return $ CharAtom' c
annotateExpression UnspecifiedAtom _ = return $ UnspecifiedAtom'
-- Internal defines OUTSIDE of a body have no meaning
annotateExpression (DefineNode _ _) _ =  error "No define allowed in current context"
annotateExpression (DefineSyntaxNode _ _) _ = error "No define-syntax allowed in current context"

annotateLambda :: MetaNode -> State AnalysisState MetaNode'
annotateLambda (LambdaNode params variadic exprs) = do
    let exprs' = injectNames (params ++ [variadic]) exprs
    let names =  unpackName' <$> (params ++ [variadic])
    pushActivationEntries names
    body' <- annotateBody exprs' True
    popEntries
    if (\(IdentifierAtom str 0) -> null str) variadic then
        return $ LambdaNode' (length params) False  body'
    else
        return $ LambdaNode' (length params) True body'
    where
        unpackName' :: MetaNode -> String
        unpackName' (IdentifierAtom str _)  = str

injectNames :: [MetaNode] -> [MetaNode] -> [MetaNode]
injectNames names exprs
  = foldl (\ exprs name -> injectName name <$> exprs) exprs names

injectName :: MetaNode -> MetaNode ->  MetaNode
injectName (IdentifierAtom str 0) mn = mn
injectName id@(IdentifierAtom str n) (LambdaNode params var body) = LambdaNode (injectName id <$> params) (injectName id var) (injectName id <$> body)
injectName id@(IdentifierAtom str n) (LetSyntaxNode binding body) = LetSyntaxNode (injectName id binding) (injectName id <$> body)
injectName id@(IdentifierAtom str n) (LetrecSyntaxNode binding body) = LetrecSyntaxNode (injectName id binding) (injectName id <$> body)
injectName (IdentifierAtom str n) id@(DefineSyntaxNode src trg) = DefineSyntaxNode (injectName id src) (injectName id trg)
injectName (IdentifierAtom str n) id@(DefineNode src trg)  = DefineNode (injectName id src) (injectName id trg)
injectName (IdentifierAtom str n) (ApplicationNode mn mns)  = ApplicationNode (injectName (IdentifierAtom str n) mn) (injectName (IdentifierAtom str n) <$> mns)
injectName (IdentifierAtom str n) (PairNode car cdr)  = let
    car' = injectName (IdentifierAtom str n) car
    cdr' = injectName (IdentifierAtom str n) cdr in
        PairNode car' cdr'
injectName (IdentifierAtom str n) (IfNode ifexpr thenbr elsebr)  = let
    ifexpr' = injectName (IdentifierAtom str n) ifexpr
    thenbr' = injectName (IdentifierAtom str n) thenbr
    elsebr' = injectName (IdentifierAtom str n) elsebr in
        IfNode ifexpr' thenbr' elsebr'
injectName (IdentifierAtom str n) (SetNode src trg)  = let
    src' = injectName (IdentifierAtom str n) src
    trg' = injectName (IdentifierAtom str n) trg in
        SetNode src' trg'
injectName (IdentifierAtom str n) (IdentifierAtom str' n')
    | str == str' && n == n' = IdentifierAtom str 0
    | otherwise = IdentifierAtom str' n'
injectName (IdentifierAtom str n) (NumberAtom z) = NumberAtom z
injectName (IdentifierAtom str n) EmptyAtom  = EmptyAtom
injectName (IdentifierAtom str n) (StringAtom cs) = StringAtom cs
injectName (IdentifierAtom str n) (BoolAtom b) = BoolAtom b
injectName (IdentifierAtom str n) (CharAtom c) = CharAtom c
injectName (IdentifierAtom str n) UnspecifiedAtom = UnspecifiedAtom


-- Makro params need to be annotated
annotateApplication :: MetaNode -> Bool -> State AnalysisState MetaNode'
annotateApplication (ApplicationNode mn params) tailpos = do
    mn' <- annotateExpression mn False
    case mn' of
        (BaseSyntaxAtom' str level) -> do
            pushScope level
            mn' <- annotateExpression (makroengineBase str (ApplicationNode mn params)) tailpos
            popEntries
            return mn'
        (SyntaxAtom' f level) -> do
            pushScope level
            mn' <- annotateExpression (f (ApplicationNode mn params)) tailpos
            popEntries
            return mn'
        _ -> do
            params' <- annotateList params False
            return $ ApplicationNode' tailpos mn' params'

annotateIdentifier :: MetaNode -> State AnalysisState MetaNode'
annotateIdentifier (IdentifierAtom str scope) = resolveIdentifier' 0 0 0 scope where
    resolveIdentifier' :: Int -> Int -> Int -> Int -> State AnalysisState MetaNode'
    resolveIdentifier' parent level 0 0 = do
        entry <- get
        id <- case entry of
                Activation ls Global -> do
                    case lookup str ls of
                        Nothing -> do
                            put Global
                            resolveIdentifier' (parent+1) (level+1) 0 0
                        (Just p) -> return $ GlobalAtom' p
                Activation ls tb -> do
                    case lookup str ls of
                        Nothing -> do
                            put tb
                            resolveIdentifier' (parent+1) (level+1) 0 0
                        (Just p) -> return $ BoundAtom' parent p
                Syntax ls tb -> do
                    case lookup str ls of
                        Nothing -> do
                            put tb
                            resolveIdentifier' parent (level+1) 0 0
                        (Just f) -> return $ SyntaxAtom' f level
                Scope n tb -> do
                    put tb
                    resolveIdentifier' parent (level+1) n 0
                Global -> return $ resolveBaseIdentifier'' str
        put entry
        return id
        where
            resolveBaseIdentifier'' :: String  -> MetaNode'
            resolveBaseIdentifier'' str
                | str `elem` schemeMakros = BaseSyntaxAtom' str level
                | str `elem` schemeFunctions = BaseFunctionAtom' str
                | otherwise = error $ "Unbound Symbol: Identifier " ++ str ++ " is not in scope"
    resolveIdentifier' parent level ignore 0 = do
        entry <- get
        id <- case entry of
                Activation _ tb -> do
                    put tb
                    resolveIdentifier' (parent+1) (level+1) (ignore-1) 0
                Syntax _ tb -> do
                    put tb
                    resolveIdentifier' parent (level+1) (ignore-1) 0
                Scope _ tb -> do
                    put tb
                    resolveIdentifier' parent (level+1) (ignore-1) 0
                Global -> error $ "Unbound Symbol: Identifier " ++ str ++ " is not in scope"
        put entry
        return id
    resolveIdentifier' parent level ignore scope = do
        entry <- get
        id <- case entry of
                Activation _ tb -> do
                    put tb
                    resolveIdentifier' (parent+1) (level+1) ignore scope
                Syntax _ tb -> do
                    put tb
                    resolveIdentifier' parent (level+1) ignore scope
                Scope _ tb -> do
                    put tb
                    resolveIdentifier' parent (level+1) ignore (scope-1)
                Global -> error "Internal Compiler Error: Invalid scope specified"
        put entry
        return id

annotateLetSyntax :: MetaNode -> Bool -> State AnalysisState MetaNode'
annotateLetSyntax (LetSyntaxNode rules body) tailpos = do
    let identifiers = makroengineLetIdentifiers rules
    let rules' = foldr injectName rules identifiers
    pushSyntaxEntries (makroengineLet rules')
    body' <- annotateBody (injectNames identifiers body) tailpos
    popEntries
    return body'
annotateLetrecSyntax :: MetaNode -> Bool -> State AnalysisState MetaNode'
annotateLetrecSyntax (LetrecSyntaxNode rules body) tailpos = do
    let identifiers = makroengineLetIdentifiers rules
    let rules' = foldr injectName rules identifiers
    pushSyntaxEntries (makroengineLet rules')
    body' <- annotateBody (injectNames identifiers body) tailpos
    popEntries
    return body'
