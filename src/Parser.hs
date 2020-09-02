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


p1 = [POpen, Identifier "+", Integral 2, Integral 5, PClose]
p2 = [POpen, POpen, Lambda,  POpen, Identifier "x", Identifier "y", Point, Identifier "z",PClose,
    POpen, Identifier "/", Identifier "x", Identifier "y", PClose, PClose, Integral 3, Integral 9, Integral 4, String "sdf", PClose]
p3 = [POpen, Quote, POpen, Identifier "*", Integral 2, Identifier "a",PClose, PClose]
p4 = [ShortQuote, POpen, Identifier "*", Integral 2, POpen, Integral 3, Integral 5, PClose ,Identifier "a",PClose]
p5 = [POpen, QuasiQuote, POpen, Identifier "*", Integral 4, POpen, Unquote, POpen, Identifier "+",
    Real 3.4, Identifier "PI", PClose,PClose, Identifier "t", PClose, PClose]

parseScheme :: [Token] -> MetaNode
parseScheme st = case runState (parse "Scheme Program")  st of
    (mn, []) -> mn
    (_, t:_) -> error $ "Unexpected token " ++ show t ++  "not allowed in current context"

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
    ts <- get
    case ts of
        (t:ts) -> do
            put ts
            if  t ==  c then
                return ()
            else
                error $ (show c) ++ "in " ++ context ++ " expected, but not found"
        _ -> error $ "Unexpected end of token stream in " ++ context

peek :: String -> State [Token] Token
peek context = do
    ts <- get
    case ts of
        (t:ts) -> return t 
        _ -> error $ "Unexpected end of token stream in " ++ context 

parse :: String -> State [Token] MetaNode
parse context = do
    t <- peek context
    case t of
        POpen -> parseSyntax
        ShortQuote -> do
            pullEq "Datum" ShortQuote
            parseDatum
        ShortQuasiQuote -> do
            pullEq "Quasiquoted Datum" ShortQuasiQuote
            parseQuasiQuotedDatum
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
        _ -> parseApplication

parseAtom :: State [Token] MetaNode
parseAtom = do 
    t <- pull "Atom"
    case t of  
        Bool b -> return $ BoolAtom b
        String s -> return $ StringAtom s
        Real n -> return $ RealAtom n
        Integral n -> return $ IntegralAtom n
        Rational n m -> return $ RationalAtom n m
        Complex c -> return $ ComplexAtom c
        Identifier i -> return $ IdentifierAtom i
        Quote -> return $ IdentifierAtom "Quote"
        Unquote -> return $ IdentifierAtom "Unquote"
        QuasiQuote -> return $ IdentifierAtom "Quasiquote"
        _ -> error $ "Unexpected Token " ++ show t ++ " not allowed in current context"

parseQuote :: State [Token] MetaNode
parseQuote = do
    pullEq "Quote" Quote
    d <- parseDatum
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
    (c, l) <- parseParams
    e <- parse "Lambda Body"
    pullEq "Lambda" PClose
    return (LambdaNode c l e) 

parseIf :: State [Token] MetaNode
parseIf = do
    pullEq "If" If
    p <- parse "If Condition"
    a <- parse "If Then Branch"
    b <- parse "If Else Branch"
    pullEq "If" PClose
    return (IfNode p a b)  

parseSet :: State [Token] MetaNode
parseSet = do
    pullEq "Set" Set
    t <- pull "Set"
    case t of 
        (Identifier str) -> do
            e <- parse "Set Body"
            return (SetNode (IdentifierAtom str) e)
        _ -> error "Expected Identifier as first argument of set"

parseDefine :: State [Token] MetaNode
parseDefine = do
    pullEq "Define" Define
    t <- pull "Define"
    case t of 
        (Identifier str) -> do
            e <- parse "Define Body"
            return (DefineNode (IdentifierAtom str) e)
        _ -> error "Expected Identifier as first argument of define"


parseApplication :: State [Token] MetaNode
parseApplication = do
    f <- parse "Application"
    arg <- parseList
    return (ApplicationNode f arg)

parseDatum :: State [Token] MetaNode
parseDatum = do
    t <- peek "Datum"
    case t of        
        POpen -> do
            pullEq "Compound Datum" POpen
            ListNode <$> parseCompoundDatum
        ShortQuote -> parseQuotedShortForm
        ShortUnquote -> parseQuotedShortForm
        ShortQuasiQuote -> parseQuotedShortForm
        _ -> parseAtom

parseCompoundDatum :: State [Token] [MetaNode]
parseCompoundDatum = do
    t <- peek "Compound Datum"
    case t of 
        PClose -> do
            pullEq "Compound Datum" PClose
            return []
        _ -> do
            e <- parseDatum
            es <- parseCompoundDatum
            return (e:es)

parseQuotedShortForm :: State [Token] MetaNode
parseQuotedShortForm = do
    t <- pull "Quoted Shortform"
    ListNode <$> ((\x -> [(IdentifierAtom (show t)),x]) <$> parseDatum)

parseQuasiQuotedDatum :: State [Token] MetaNode
parseQuasiQuotedDatum = do
    t <- peek "Quasiquoted Datum"
    case t of        
        POpen -> do
            pullEq "Quasiquoted Datum" POpen
            t <- peek "Quasiquoted Datum"
            case t of
                Unquote -> do
                    pullEq "Unquoted Expression" Unquote 
                    e <- parse "Unquoted Expression"
                    pullEq "Unquoted Expression" PClose
                    return e
                _ -> ListNode <$> parseQuasiQuotedCompoundDatum
        ShortUnquote -> do
            pullEq "Unquoted Expression" ShortUnquote
            parse "Unquoted Expression"
        ShortQuote -> parseQuasiQuotedShortForm
        ShortQuasiQuote -> parseQuasiQuotedShortForm
        _ -> parseAtom

parseQuasiQuotedCompoundDatum :: State [Token] [MetaNode]
parseQuasiQuotedCompoundDatum = do
    t <- peek "Quasiquoted Compound Datum"
    case t of 
        PClose -> do
            pullEq "Quasiquoted Compound Datum" PClose
            return []
        _ -> do
            e <- parseQuasiQuotedDatum
            es <- parseQuasiQuotedCompoundDatum
            return (e:es)

parseQuasiQuotedShortForm :: State [Token] MetaNode
parseQuasiQuotedShortForm = do
    t <- pull "Quasiquoted Shortform"
    ListNode <$> ((\x -> [(IdentifierAtom (show t)),x]) <$> parseQuasiQuotedDatum)

parseList :: State [Token] [MetaNode]
parseList = do
    t <- peek "List"
    case t of 
        PClose -> do
            pullEq "List" PClose
            return []
        _ -> do
            e <- parse "List Element"
            es <- parseList
            return (e:es)

parseParams :: State [Token] ([MetaNode], MetaNode)
parseParams = do
    t <- pull "Formal Paramters"
    case t of
        POpen -> parseIdentifierList
        Identifier s -> return ([],IdentifierAtom s)  
        _ -> error "Expected parameter list or single parameter in lambda definition"

parseIdentifierList :: State [Token] ([MetaNode], MetaNode)
parseIdentifierList = do 
    t <- pull "Formal Paramters"
    case t of 
        PClose -> do
            return ([], IdentifierAtom "")
        Point -> do
            t <- pull "Formal Paramters"
            case t of 
                Identifier str -> do
                    pullEq "Formal Paramters" PClose
                    return ([], IdentifierAtom str)
                _ -> error $ "Expected parameter list or single parameter in lambda definition, not token " ++ show t
        Identifier str -> do
            (is, i) <- parseIdentifierList
            return ((IdentifierAtom str):is, i)
        _ -> error $ "Expected parameter list or single parameter in lambda definition, not token " ++ show t
                
