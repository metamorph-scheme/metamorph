module SemanticAnalysis.SemanticAnalysis (
    semanticAnalysis
) where

import Parser.MetaNode
import Control.Monad.State.Lazy
import Common.Number
import SemanticAnalysis.Testdata
import SemanticAnalysis.MetaNode'
import Parser.MetaNode


type AnalysisState = SymbolTable

makroengineBase :: String -> (MetaNode -> MetaNode)
makroengineBase = error "Makroengine not implemented"
makroengineDefine :: MetaNode -> [(String, MetaNode -> MetaNode)]
makroengineDefine = error "Makroengine not implemented"
makroengineLet :: MetaNode -> [(String, MetaNode -> MetaNode)]
makroengineLet = error "Makroengine not implemented"
makroengineLetrec :: MetaNode -> [(String, MetaNode -> MetaNode)]
makroengineLetrec = error "Makroengine not implemented"

pushActivationEntries :: [String] -> State AnalysisState ()
pushActivationEntries params = do
    let newentries = (zip (filter (not . null) params) [0..])
    oldentries <- get
    put (Activation newentries oldentries)

pushSyntaxEntries :: [(String, MetaNode->MetaNode)] -> State AnalysisState ()
pushSyntaxEntries newentries = do
    oldentries <- get
    put (Syntax newentries oldentries)

pushIgnoreEntries :: Int -> State AnalysisState ()
pushIgnoreEntries n = do
    entries <- get
    put (IgnoreNext n entries)

popEntries :: State AnalysisState ()
popEntries = do
    entries <- get
    case entries of
        Activation _ table -> put table
        Syntax _ table -> put table
        IgnoreNext _ table -> put table
        Global -> error "Internal Compiler Error: popEntries on empty symbol table"

initSymbolTable :: [MetaNode] -> State AnalysisState [MetaNode]
initSymbolTable ((DefineNode (IdentifierAtom str) trg):ms) = do
    oldentries <- get
    case oldentries of
        Activation entries next -> do
            put $ Activation ((str, length entries):entries) next
            ms' <- initSymbolTable ms
            return $ (SetNode (IdentifierAtom str) trg):ms'
        _ -> error "Internal Compiler Error: Incorrect Usage of initSymbolTable"
initSymbolTable (m:ms) = do
    ms' <- initSymbolTable ms
    return (m:ms')
initSymbolTable [] = return []

semanticAnalysis :: [MetaNode] -> MetaNode'
semanticAnalysis mn = 
    evalState (annotateBody mn False) Global

annotateBody :: [MetaNode] -> Bool -> State AnalysisState MetaNode'
annotateBody mn tail = do
    pushActivationEntries []
    mn' <- initSymbolTable mn
    tb <- get
    case tb of 
        -- dont count empty bodies
        (Activation [] _) -> do
            popEntries -- bound numbers will not include empty body activation
            mn'' <- annotateList mn' tail
            return $ BodyNode' 0 mn''
        (Activation entries _) -> do
            mn'' <- annotateList mn' tail
            popEntries
            return $ BodyNode' (length entries) mn''
        _ -> error "Internal Compiler Error: Faulty symbol table"

annotateList :: [MetaNode] -> Bool -> State AnalysisState [MetaNode']
annotateList [] _ = return []
annotateList (definesyntax@(DefineSyntaxNode _ _):es) tail = do 
    pushSyntaxEntries (makroengineDefine definesyntax)
    es' <- annotateList es tail
    popEntries
    return es'
annotateList [e] tail = do 
    e' <- annotateExpression e tail
    return [e']
annotateList (e:es) tail = do 
    e' <- annotateExpression e False
    es' <- annotateList es tail
    return $ e':es'

annotateExpression :: MetaNode -> Bool -> State AnalysisState MetaNode'
annotateExpression lambda@(LambdaNode _ _ _) _ = annotateLambda lambda
annotateExpression application@(ApplicationNode _ _) tail = annotateApplication application tail
annotateExpression identifier@(IdentifierAtom _) _ = annotateIdentifier identifier
annotateExpression letsyntax@(LetSyntaxNode _ _ ) tail = annotateLetSyntax letsyntax tail
annotateExpression letrecsyntax@(LetrecSyntaxNode _ _) tail = annotateLetrecSyntax letrecsyntax tail
-- Recursive Trivial Annotations
annotateExpression (PairNode car cdr) _ = do
    car' <- annotateExpression car False
    cdr' <- annotateExpression cdr False
    return $ PairNode' car' cdr'
annotateExpression (IfNode ifexpr thenbr elsebr) tail = do
    ifexpr' <- annotateExpression ifexpr False
    thenbr' <- annotateExpression thenbr tail
    elsebr' <- annotateExpression elsebr tail
    return $ IfNode' ifexpr' thenbr' elsebr'
annotateExpression (SetNode src trg) _ = do
    src' <- annotateExpression src False
    trg' <- annotateExpression trg False
    return $ SetNode' src' trg'
-- Trivial Annotations
annotateExpression (NumberAtom n) _ = return $ NumberAtom' n
annotateExpression EmptyAtom _ = return $ EmptyAtom'
annotateExpression (StringAtom cs) _ = return $ StringAtom' cs
annotateExpression (BoolAtom b) _ = return $ BoolAtom' b
annotateExpression (CharAtom c) _ = return $ CharAtom' c
-- Internal defines OUTSIDE of a body have no meaning
annotateExpression (DefineNode _ _) _ =  return $ BodyNode' 0 []
annotateExpression (DefineSyntaxNode _ _) _ = return $ BodyNode' 0 []

annotateLambda :: MetaNode -> State AnalysisState MetaNode'
annotateLambda (LambdaNode (params) variadic exprs) = do
    pushActivationEntries $ ((\(IdentifierAtom str) -> str) <$> (params ++ [variadic]))
    body' <- annotateBody exprs True
    popEntries
    if (\(IdentifierAtom str) -> null str) variadic then
        return $ LambdaNode' (length params) False  body'
    else
        return $ LambdaNode' (length params) True body'

-- Makro params need to be annotated
annotateApplication :: MetaNode -> Bool -> State AnalysisState MetaNode'
annotateApplication (ApplicationNode mn params) tail = do
    mn' <- annotateExpression mn False
    case mn' of
        (BaseSyntaxAtom' str) -> do
            symtable <- get
            put Global 
            mn' <- annotateExpression ((makroengineBase str) (ApplicationNode mn params)) tail
            put symtable
            return mn'
        (SyntaxAtom' f level) -> do
            pushIgnoreEntries level
            mn' <- annotateExpression (f (ApplicationNode mn params)) tail
            popEntries
            return mn'
        _ -> do
            params' <- annotateList params False
            return $ ApplicationNode' tail mn' params'

annotateIdentifier :: MetaNode -> State AnalysisState MetaNode'
annotateIdentifier (IdentifierAtom str)
    | elem str schemeMakros = return $ BaseSyntaxAtom' str
    | elem str schemeFunctions =  return $ BaseFunctionAtom' str
    | otherwise = resolveIdentifier str 0 0 0

resolveIdentifier :: String -> Int -> Int -> Int -> State AnalysisState MetaNode'
resolveIdentifier str parent level 0 = do
    entry <- get
    id <- case entry of
            Activation ls Global -> do
                case lookup str ls of
                    Nothing -> do 
                        put Global 
                        resolveIdentifier str (parent+1) (level+1) 0
                    (Just p) -> return $ GlobalAtom' p
            Activation ls tb -> do
                case lookup str ls of
                    Nothing -> do 
                        put tb
                        resolveIdentifier str (parent+1) (level+1) 0
                    (Just p) -> return $ BoundAtom' parent p
            Syntax ls tb -> do
                case lookup str ls of
                    Nothing -> do 
                        put tb
                        resolveIdentifier str parent (level+1) 0
                    (Just f) -> return $ SyntaxAtom' f level
            IgnoreNext n tb -> do
                put tb
                resolveIdentifier str parent level n
            Global -> error $ "Unbound Symbol: Identifier " ++ str ++ " is not in scope"
    put entry
    return id

resolveIdentifier str parent level ignore = do
    entry <- get
    id <- case entry of
            Activation _ tb -> do
                put tb
                resolveIdentifier str (parent+1) (level+1) (ignore-1)
            Syntax _ tb -> do 
                put tb
                resolveIdentifier str parent (level+1) (ignore-1)
            IgnoreNext _ tb -> do
                put tb
                resolveIdentifier str parent level (ignore-1)
            Global -> error "Internal Compiler Error: To high ignore value"
    put entry
    return id

annotateLetSyntax :: MetaNode -> Bool -> State AnalysisState MetaNode'
annotateLetSyntax (LetSyntaxNode rules body) tail = do
    pushSyntaxEntries (makroengineLet rules)
    body' <- annotateBody body tail
    popEntries
    return body'

annotateLetrecSyntax :: MetaNode -> Bool -> State AnalysisState MetaNode'
annotateLetrecSyntax (LetrecSyntaxNode rules body) tail = do
    pushSyntaxEntries (makroengineLetrec rules)
    body' <- annotateBody body tail
    popEntries
    return body'