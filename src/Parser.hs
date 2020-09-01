module Parser where

import Control.Monad.State.Lazy
import Data.Data
import Data.Complex

data Token  = Lambda | If | Set | POpen | PClose | Identifier String | Quote | ShortQuote | Integral Int | Rational Int Int
            | Real Double | String String | Complex (Complex Double)  | Bool Bool | Char Char
            | Point | QuasiQuote | ShortQuasiQuote | Unquote | ShortUnquote | UnquoteSplice 
            | ShortUnquoteSplice | Label Integer | LabelRef Integer | Define deriving (Eq, Show) 

data MetaNode = LambdaNode [MetaNode] MetaNode MetaNode 
            | ListNode [MetaNode] | RealAtom Double | IntegralAtom Int | RationalAtom Int Int 
            | StringAtom String | ComplexAtom (Complex Double) | BoolAtom Bool | CharAtom Char 
            | IdentifierAtom String | ApplicationNode MetaNode [MetaNode] 
            | IfNode MetaNode MetaNode MetaNode | SetNode MetaNode MetaNode | DefineNode MetaNode MetaNode 
            deriving Show 

push :: Token -> State [Token] ()
push t = do
    ts <- get
    put (t:ts)

pull :: State [Token] Token
pull = do
    ts <- get
    case ts of 
        (t:ts) -> do
            put ts
            return t
        _ -> error "Unexpected end of token stream"

pullEq :: Token -> State [Token] ()
pullEq c = do
    ts <- get
    case ts of
        (t:ts) -> do
            put ts
            if  t ==  c then
                return ()
            else
                error $ (show c) ++ " expected, but not found"
        _ -> error "Unexpected end of token stream"

parse :: State [Token] MetaNode
parse = do
    t <- pull
    case t of
        POpen -> parseExpr
        ShortQuote -> parseQuotedDatum
        ShortQuasiQuote -> parseQuasiQuotedDatum
        Bool b -> return $ BoolAtom b
        String s -> return $ StringAtom s
        Real n -> return $ RealAtom n
        Integral n -> return $ IntegralAtom n
        Rational n m -> return $ RationalAtom n m
        Complex c -> return $ ComplexAtom c
        Identifier i -> return $ IdentifierAtom i

parseExpr :: State [Token] MetaNode
parseExpr = do 
    t <- pull
    case t of
        Lambda -> parseLambda
        Define -> parseDefine
        Quote -> parseQuote
        QuasiQuote -> parseQuasiQuote
        If -> parseIf
        Set -> parseSet
        _ -> do
            push t
            parseApplication

parseQuote :: State [Token] MetaNode
parseQuote = do
    d <- parseQuotedDatum
    pullEq PClose
    return d

parseQuasiQuote :: State [Token] MetaNode
parseQuasiQuote = do
    d <- parseQuasiQuotedDatum
    pullEq PClose
    return d

parseLambda :: State [Token] MetaNode
parseLambda = do
    (c, l) <- parseParams
    e <- parse
    pullEq PClose
    return (LambdaNode c l e) 

parseIf :: State [Token] MetaNode
parseIf = do
    p <- parse 
    a <- parse 
    b <- parse
    pullEq PClose
    return (IfNode p a b)  

parseSet :: State [Token] MetaNode
parseSet = do
    t <- pull
    case t of 
        (Identifier str) -> do
            e <- parse
            return (SetNode (IdentifierAtom str) e)
        _ -> error "Expected Identifier"

parseDefine :: State [Token] MetaNode
parseDefine = do
    t <- pull
    case t of 
        (Identifier str) -> do
            e <- parse
            return (DefineNode (IdentifierAtom str) e)
        _ -> error "Expected Identifier"


parseApplication :: State [Token] MetaNode
parseApplication = do
    f <- parse
    arg <- parseList
    return (ApplicationNode f arg)

parseQuotedDatum :: State [Token] MetaNode
parseQuotedDatum = do
    t <- pull
    case t of        
        POpen -> ListNode <$> parseListQuotedDatum
        ShortQuote -> ListNode <$> ((\x -> [(IdentifierAtom "Quote"),x]) <$> parseQuotedDatum)
        ShortQuasiQuote -> ListNode <$> ((\x -> [(IdentifierAtom "Quasiquote"),x]) <$> parseQuotedDatum)
        ShortUnquote -> ListNode <$> ((\x -> [(IdentifierAtom "Unquote"),x]) <$> parseQuotedDatum)
        Quote -> return $ IdentifierAtom "Quote"
        Unquote -> return $ IdentifierAtom "Unquote"
        QuasiQuote -> return $ IdentifierAtom "Quasiquote"
        Bool b -> return $ BoolAtom b
        String s -> return $ StringAtom s
        Real n -> return $ RealAtom n
        Integral n -> return $ IntegralAtom n
        Rational n m -> return $ RationalAtom n m
        Complex c -> return $ ComplexAtom c
        Identifier i -> return $ IdentifierAtom i

parseQuasiQuotedDatum :: State [Token] MetaNode
parseQuasiQuotedDatum = do
    t <- pull
    case t of        
        POpen -> do
            t <- pull
            case t of
                Unquote -> do
                    e <- parse
                    pullEq PClose
                    return e
                _ -> do
                    push t
                    ListNode <$> parseListQuasiQuotedDatum
        ShortQuote -> ListNode <$> ((\x -> [(IdentifierAtom "Quote"),x]) <$> parseQuasiQuotedDatum)
        ShortQuasiQuote -> ListNode <$> ((\x -> [(IdentifierAtom "Quasiquote"),x]) <$> parseQuasiQuotedDatum)
        ShortUnquote -> parse
        Quote -> return $ IdentifierAtom "Quote"
        Unquote -> return $ IdentifierAtom "Unquote"
        QuasiQuote -> return $ IdentifierAtom "Quasiquote"
        Bool b -> return $ BoolAtom b
        String s -> return $ StringAtom s
        Real n -> return $ RealAtom n
        Integral n -> return $ IntegralAtom n
        Rational n m -> return $ RationalAtom n m
        Complex c -> return $ ComplexAtom c
        Identifier i -> return $ IdentifierAtom i

parseListQuasiQuotedDatum :: State [Token] [MetaNode]
parseListQuasiQuotedDatum = do
    ts <- get
    case ts of 
        (t:ts) ->
            case t of 
                PClose -> do
                    pull
                    return []
                _ -> do
                    e <- parseQuasiQuotedDatum
                    es <- parseListQuasiQuotedDatum
                    return (e:es)
        _ -> error "Unexpected end of tokenstream"

parseListQuotedDatum :: State [Token] [MetaNode]
parseListQuotedDatum = do
    ts <- get
    case ts of 
        (t:ts) ->
            case t of 
                PClose -> do
                    pull
                    return []
                _ -> do
                    e <- parseQuotedDatum
                    es <- parseListQuotedDatum
                    return (e:es)
        _ -> error "Unexpected end of tokenstream"

parseList :: State [Token] [MetaNode]
parseList = do
    ts <- get
    case ts of 
        (t:ts) ->
            case t of 
                PClose -> do
                    pull
                    return []
                _ -> do
                    e <- parse
                    es <- parseList
                    return (e:es)
        _ -> error "Unexpected end of tokenstream"

parseParams :: State [Token] ([MetaNode], MetaNode)
parseParams = do
    t <- pull
    case t of
        POpen -> parseIdentifierList
        Identifier s -> return ([],IdentifierAtom s)  
        _ -> error "Expected parameter list or single parameter"

parseIdentifierList :: State [Token] ([MetaNode], MetaNode)
parseIdentifierList = do 
    ts <- get
    case ts of
        (t:ts) ->
            case t of 
                PClose -> do
                    pull
                    return ([], IdentifierAtom "")
                Point -> do
                    pull
                    ts <- get
                    case ts of
                        (t:ts) ->
                            case t of 
                                Identifier str -> do
                                    pull
                                    pullEq PClose
                                    return ([], IdentifierAtom str)
                                _ -> error "In a lambda parameter list are only identifiers allowed"
                        _ -> error "Unexpected end of tokenstream"    
                Identifier str -> do
                    pull
                    (is, i) <- parseIdentifierList
                    return ((IdentifierAtom str):is, i)
                _ -> error "In a lambda parameter list are only identifiers allowed"
        _ -> error "Unexpected end of token stream"
                
