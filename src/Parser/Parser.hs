module Parser.Parser (
    parseScheme
) 
where

import Parser.MetaNode
import Control.Monad.State.Lazy
import Data.Data
import Common.Number
import Data.Complex
import Lexer.Token


parseScheme :: [Token] -> [MetaNode]
parseScheme st = case runState (parseExpression "Scheme Program") st of
    (mn, []) -> [mn]
    (mn, up) -> mn:parseScheme up
    --(_, t:_) -> error $ "Unexpected token " ++ show t ++ " not allowed in top level of program"

push :: Token -> State [Token] ()
push t = do
    ts <- get
    put (t:ts)

pull :: String -> State [Token] Token
pull context = do
    ts <- get
    case ts of 
        (t:ts) -> do
            put ts
            return t
        _ -> error $ "Unexpected end of token stream in " ++ context

pullEq :: String -> Token -> State [Token] ()
pullEq context c = do
    t <- pull context
    if  t ==  c then
        return ()
    else
        error $ (show c) ++ " in " ++ context ++ " expected, but not found"


peek :: String -> State [Token] Token
peek context = do
    ts <- get
    case ts of
        (t:ts) -> return t 
        _ -> error $ "Unexpected end of token stream in " ++ context 

parseExpression :: String -> State [Token] MetaNode
parseExpression context = do
    t <- peek context
    case t of
        POpen -> parseSyntax
        ShortQuote -> do
            pullEq "Datum" ShortQuote
            parseQuotedDatum
        ShortQuasiQuote -> do
            pullEq "Quasiquoted Datum" ShortQuasiQuote
            parseQuasiQuotedDatum
        CommentDatum -> do
            pullEq "Commented Datum" CommentDatum
            parseExpression "Commented Datum"
            parseExpression context
        _ -> parseAtom

parseSyntax :: State [Token] MetaNode
parseSyntax = do
    pullEq "Syntactic construct" POpen 
    t <- peek "Syntactic construct"
    case t of
        Lambda -> parseLambda
        Define -> parseDefine
        Quote -> parseQuote
        QuasiQuote -> parseQuasiQuote
        If -> parseIf
        Set -> parseSet
        PClose -> do
            pullEq "PClose" PClose
            return EmptyAtom
        DefineSyntax -> parseDefineSyntax
        LetSyntax -> parseLetSyntax
        LetrecSyntax -> parseLetrecSyntax
        _ -> parseApplication

parseAtom :: State [Token] MetaNode
parseAtom = do 
    t <- pull "Atom"
    case t of  
        Bool b -> return $ BoolAtom b
        String s -> return $ StringAtom s
        Char c -> return $ CharAtom c
        Number n -> return $ NumberAtom n
        Identifier i -> return $ IdentifierAtom i 0
        Quote -> return $ IdentifierAtom "quote" 0
        Unquote -> return $ IdentifierAtom "unquote" 0
        QuasiQuote -> return $ IdentifierAtom "quasiquote" 0
        Set  -> return $ IdentifierAtom "set!" 0
        Define  -> return $ IdentifierAtom "define" 0
        _ -> error $ "Unexpected token " ++ show t ++ " not allowed in current context"

parseQuote :: State [Token] MetaNode
parseQuote = do
    pullEq "Quote" Quote
    d <- parseQuotedDatum
    pullEq "Quote" PClose
    return d

parseQuasiQuote :: State [Token] MetaNode
parseQuasiQuote = do
    pullEq "Quasiquote" QuasiQuote
    d <- parseQuasiQuotedDatum
    pullEq "Quasiquote" PClose
    return d

parseLambda :: State [Token] MetaNode
parseLambda = do
    pullEq "Lambda" Lambda
    (c, l) <- parseFormalParameters
    e <- parseExpressionList "Body"
    return (LambdaNode c l e) 

parseIf :: State [Token] MetaNode
parseIf = do
    pullEq "If" If
    p <- parseExpression "If Condition"
    a <- parseExpression "If Then Branch"
    next <- peek "If"
    if next /= PClose then do
        b <- parseExpression "If Else Branch"
        pullEq "If" PClose
        return (IfNode p a b) 
    else do
        pullEq "If" PClose
        return (IfNode p a UnspecifiedAtom)

parseSet :: State [Token] MetaNode
parseSet = do
    pullEq "Set" Set
    t <- pull "Set"
    case t of 
        (Identifier str) -> do
            e <- parseExpression "Set Body"
            pullEq "Set" PClose
            return (SetNode (IdentifierAtom str 0) e)
        _ -> error "Expected Identifier as first argument of set"

parseDefine :: State [Token] MetaNode
parseDefine = do
    pullEq "Define" Define
    t <- pull "Define"
    case t of 
        (Identifier str) -> do
            p <- peek "Define"
            if p /= PClose then do
                e <- parseExpression "Define Body"
                pullEq "Define" PClose
                return (DefineNode (IdentifierAtom str 0) e)
            else do
                pullEq "Define" PClose
                return (DefineNode (IdentifierAtom str 0) UnspecifiedAtom)
        POpen -> parseDefineFunction
        _ -> error "Expected Identifier oder Parantheses as first argument of define"

parseDefineFunction :: State [Token] MetaNode
parseDefineFunction =  do
            (ps, p) <- parseFormalParameterList
            es <- parseExpressionList "Define Body"
            case ps of
                (i:ps) -> return (DefineNode i (LambdaNode ps p es))
                _ -> error "Expected parameter list in function definition"

parseDefineSyntax :: State [Token] MetaNode
parseDefineSyntax = do
    pullEq "Define-Syntax" DefineSyntax
    t <- pull "Define-Syntax"   
    case t of
        (Identifier str) -> do
            e <- parseExpression "Define-Syntax Rules"
            pullEq "Define-Syntax" PClose
            return (DefineSyntaxNode (IdentifierAtom str 0) e)
        _ -> error "Expected Identifier as first argument of define-syntax"

parseLetSyntax :: State [Token] MetaNode
parseLetSyntax = do
    pullEq "Let-Syntax" LetSyntax
    e <- parseExpression "Let-Syntax Rules"
    body <- parseExpressionList "Let-Syntax Body"
    return (LetSyntaxNode e body)

parseLetrecSyntax :: State [Token] MetaNode
parseLetrecSyntax = do
    pullEq "Letrec-Syntax" LetrecSyntax
    e <- parseExpression "Letrec-Syntax Rules"
    body <- parseExpressionList "Letrec-Syntax Body"
    return (LetrecSyntaxNode e body)
   

parseApplication :: State [Token] MetaNode
parseApplication = do
    f <- parseExpression "Lambda Application"
    arg <- parseExpressionList "Argument"
    return (ApplicationNode f arg)

parseQuotedDatum :: State [Token] MetaNode
parseQuotedDatum = do
    t <- peek "Datum"
    case t of        
        POpen -> do
            pullEq "Compound Datum" POpen
            parseQuotedCompoundDatum
        CommentDatum -> do
            pullEq "Commented Quoted Datum" CommentDatum
            parseQuotedDatum
            parseQuotedDatum
        ShortQuote -> parseQuotedShortForm
        ShortUnquote -> parseQuotedShortForm
        ShortQuasiQuote -> parseQuotedShortForm
        _ -> parseAtom

parseQuotedCompoundDatum :: State [Token] MetaNode
parseQuotedCompoundDatum = do
    t <- peek "Compound Datum"
    case t of 
        PClose -> do
            pullEq "Compound Datum" PClose
            return EmptyAtom
        Dot -> do
            pullEq "Compound Datum" Dot
            end <- parseQuotedDatum
            pullEq "Compound Datum" PClose
            return end
        _ -> do
            e <- parseQuotedDatum
            es <- parseQuotedCompoundDatum
            return (PairNode e es)

parseQuotedShortForm :: State [Token] MetaNode
parseQuotedShortForm = do
    t <- pull "Quoted Shortform"
    d <- parseQuotedDatum
    return $ PairNode (IdentifierAtom (show t) 0) (PairNode d EmptyAtom)

parseQuasiQuotedDatum :: State [Token] MetaNode
parseQuasiQuotedDatum = do
    t <- peek "Quasiquoted Datum"
    case t of        
        POpen -> do
            pullEq "Quasiquoted Datum" POpen 
            t <- peek "Quasiquoted Datum"
            case t of
                Unquote -> parseUnquotedExpression
                _ -> parseQuasiQuotedCompoundDatum
        ShortUnquote -> do
            pullEq "Unquoted Expression" ShortUnquote
            parseExpression "Unquoted Expression"
        CommentDatum -> do
            pullEq "Commented Quasiquoted Datum" CommentDatum
            parseQuasiQuotedDatum
            parseQuasiQuotedDatum
        ShortQuote -> parseQuasiQuotedShortForm
        ShortQuasiQuote -> parseQuasiQuotedShortForm
        _ -> parseAtom

parseQuasiQuotedCompoundDatum :: State [Token] MetaNode
parseQuasiQuotedCompoundDatum = do
    t <- peek "Quasiquoted Compound Datum"
    case t of 
        PClose -> do
            pullEq "Quasiquoted Compound Datum" PClose
            return EmptyAtom
        Dot -> do
            pullEq "Quasiquoted Compound Datum" Dot
            end <- parseQuasiQuotedDatum
            pullEq "Quasiquoted Compound Datum" PClose
            return end
        _ -> do
            e <- parseQuasiQuotedDatum
            es <- parseQuasiQuotedCompoundDatum
            return (PairNode e es)

parseQuasiQuotedShortForm :: State [Token] MetaNode
parseQuasiQuotedShortForm = do
    t <- pull "Quasiquoted Shortform"
    d <- parseQuasiQuotedDatum
    return $ PairNode (IdentifierAtom (show t) 0) (PairNode d EmptyAtom)

parseUnquotedExpression :: State [Token] MetaNode
parseUnquotedExpression = do
    pullEq "Unquoted Expression" Unquote 
    e <- parseExpression "Unquoted Expression"
    pullEq "Unquoted Expression" PClose
    return e

parseExpressionList :: String -> State [Token] [MetaNode]
parseExpressionList  context = do
    t <- peek (context ++ " List")
    case t of 
        PClose -> do
            pullEq context PClose
            return []
        _ -> do
            e <- parseExpression context
            es <- parseExpressionList (context ++ " List")
            return (e:es)


parseFormalParameters :: State [Token] ([MetaNode], MetaNode)
parseFormalParameters = do
    t <- pull "Formal Paramters"
    case t of
        POpen -> parseFormalParameterList
        Identifier s -> return ([],IdentifierAtom s 0)  
        _ -> error "Expected parameter list or single parameter in lambda definition"

parseFormalParameterList :: State [Token] ([MetaNode], MetaNode)
parseFormalParameterList = do 
    t <- pull "Formal Paramterlist"
    case t of 
        PClose -> do
            return ([], IdentifierAtom "" 0)
        Dot -> do
            t <- pull "Formal Paramterlist"
            case t of 
                Identifier str -> do
                    pullEq "Formal Paramterlist" PClose
                    return ([], IdentifierAtom str 0)
                _ -> error $ "Expected formal parameters in formal parameterlist not token " ++ show t
        Identifier str -> do
            (is, i) <- parseFormalParameterList
            return ((IdentifierAtom str 0):is, i)
        _ -> error $ "Expected formal parameters in formal parameterlist not token " ++ show t
        