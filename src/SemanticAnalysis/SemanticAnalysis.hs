module SemanticAnalysis.SemanticAnalysis (
    semanticAnalysis
) where

import Control.Monad.State.Lazy
import Common.Number
import SemanticAnalysis.MetaNode'
import Parser.MetaNode

type AnalysisState = SymbolTable

makroengineBase :: String -> (MetaNode -> MetaNode)
makroengineBase = error "Makroengine not implemented"
makroengineDefine :: MetaNode -> (String, MetaNode -> MetaNode)
makroengineDefine = error "Makroengine not implemented"
makroengineLet :: MetaNode -> [(String, MetaNode -> MetaNode)]
makroengineLet = error "Makroengine not implemented"
makroengineLetrec :: MetaNode -> [(String, MetaNode -> MetaNode)]
makroengineLetrec = error "Makroengine not implemented"

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
pushDefines ((DefineNode (IdentifierAtom str 0) trg):ms) = do
    oldentries <- get
    case oldentries of
        Activation entries next -> do
            put $ Activation ((str, length entries):entries) next
            ms' <- pushDefines ms
            return $ SetNode (IdentifierAtom str 0) trg:ms'
        _ -> error "Internal Compiler Error: Incorrect Usage of pushDefines"
pushDefines ((DefineNode _ _):ms) = error "Expected unbound identifier as first argument of define"
pushDefines (m:ms) = do
    ms' <- pushDefines ms
    return (m:ms')
pushDefines [] = return []

pushDefineSyntax :: [MetaNode] -> State AnalysisState [MetaNode]
pushDefineSyntax (definesyntax@(DefineSyntaxNode (IdentifierAtom str 0) _):ms) = do
    oldentries <- get
    case oldentries of
        Syntax entries next -> do
            put $ Syntax  (makroengineDefine definesyntax:entries) next
            pushDefineSyntax ms
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
annotateBody mn tail = do
    pushActivationEntries [] -- Activation entries need to be lower in the stack then syntax entries
    mn' <- pushDefines mn
    tb <- get
    case tb of
        -- dont count empty bodies
        (Activation [] _) -> do
            popEntries -- bound numbers will not include empty body activation
            pushSyntaxEntries [] -- makro scopes will include beginning at syntax entries
            mn'' <- pushDefineSyntax mn'
            mn''' <- annotateList mn'' tail
            popEntries -- SyntaxEntries
            return $ BodyNode' 0 mn'''
        (Activation entries _) -> do
            pushSyntaxEntries [] -- makro scopes will include beginning at syntax entries
            mn'' <- pushDefineSyntax mn'
            mn''' <- annotateList mn'' tail
            popEntries -- Syntax Entries
            popEntries -- Define Entries
            return $ BodyNode' (length entries) mn'''
        _ -> error "Internal Compiler Error: Faulty symbol table"

annotateList :: [MetaNode] -> Bool -> State AnalysisState [MetaNode']
annotateList [] _ = return []
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
annotateExpression identifier@(IdentifierAtom _ _) _ = annotateIdentifier identifier
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
-- Internal defines OUTSIDE of a body have no meaning
annotateExpression (DefineNode _ _) _ =  error "No define allowed in current context"
annotateExpression (DefineSyntaxNode _ _) _ = error "No define-syntax allowed in current context"

annotateLambda :: MetaNode -> State AnalysisState MetaNode'
annotateLambda (LambdaNode params variadic exprs) = do
    let names = unpackName' <$> (params ++ [variadic])
    pushActivationEntries names
    body' <- annotateBody exprs True
    popEntries
    if (\(IdentifierAtom str 0) -> null str) variadic then
        return $ LambdaNode' (length params) False  body'
    else
        return $ LambdaNode' (length params) True body'
    where
        unpackName' :: MetaNode -> String
        unpackName' (IdentifierAtom str 0)  = str
        unpackName' (IdentifierAtom _ _)  = error "Expected unbound identifier as parameters of lambda"

-- Makro params need to be annotated
annotateApplication :: MetaNode -> Bool -> State AnalysisState MetaNode'
annotateApplication (ApplicationNode mn params) tail = do
    mn' <- annotateExpression mn False
    case mn' of
        (BaseSyntaxAtom' str level) -> do
            pushScope level
            mn' <- annotateExpression (makroengineBase str (ApplicationNode mn params)) tail
            popEntries
            return mn'
        (SyntaxAtom' f level) -> do
            pushScope level
            mn' <- annotateExpression (f (ApplicationNode mn params)) tail
            popEntries
            return mn'
        _ -> do
            params' <- annotateList params False
            return $ ApplicationNode' tail mn' params'

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