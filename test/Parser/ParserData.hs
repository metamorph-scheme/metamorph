module Parser.ParserData where
import Lexer.Token
import Parser.MetaNode
import Common.Number

p1 = [POpen, Identifier "+", Bool True, Bool True, PClose]
p2 = [POpen, POpen, Lambda,  POpen, Identifier "x", Identifier "y", Dot, Identifier "z",PClose,
    POpen, Identifier "/", Identifier "x", Identifier "y", PClose, PClose, Bool True, Bool True, Bool True, String "sdf", PClose]
p3 = [POpen, Quote, POpen, Identifier "*", Bool True, Identifier "a",PClose, PClose]
p4 = [ShortQuote, POpen, Identifier "*", Bool True, POpen, Bool True, Bool True, PClose ,Identifier "a",PClose]
p5 = [POpen, QuasiQuote, POpen, Identifier "*", Bool True, POpen, Unquote, POpen, Identifier "+",
    Bool True, Identifier "PI", PClose,PClose, Identifier "t", PClose, PClose]
p6 = [POpen, If, Identifier "p", POpen, Identifier "*", 
     Bool True, Bool True, PClose, ShortQuote, POpen, Bool True, Bool True, PClose, PClose]
p7 = [POpen,Set, Identifier "x", POpen, Lambda, Identifier "arglist", POpen, Identifier "length"
    , Identifier "arglist", PClose,PClose, PClose]
p8 = [ShortQuasiQuote, POpen, Identifier "x", ShortUnquote, POpen, Identifier "*",
    Bool True, Bool True, PClose, POpen, Identifier "c", Dot, POpen, Bool True, Bool True,
     ShortUnquote,POpen, Identifier "+", Bool True, Bool True, PClose ,PClose, PClose,PClose]
p9 = [POpen, Lambda, Identifier "X", POpen, Identifier "*", Bool True, Bool True,
    PClose, POpen, Set, Identifier "X", Bool True, PClose, Identifier "X", PClose]
p10 = [POpen, Lambda, Identifier "X", ShortQuote, POpen, Bool True, Bool True, CommentDatum,
            POpen, Bool True, Bool True, PClose, Bool True, PClose, PClose]
p11 = [POpen, Define, POpen,Identifier "func", Identifier "x", Identifier "y", Dot, Identifier "z",
        PClose, POpen, Identifier "+", POpen, Identifier "*", Identifier "x", POpen, Identifier "len", Identifier "z",
        PClose, PClose, POpen, Identifier "*", Identifier "y", POpen, Identifier "len", Identifier "z",
        PClose, PClose,PClose,PClose]
p12 = [POpen, If, POpen, Identifier "Eq", Bool True, Bool False, PClose, POpen, Set, Identifier "x", Number (Exact (Integer 0)),PClose, PClose]



n1 = [ApplicationNode (IdentifierAtom "+" 0) [BoolAtom True,BoolAtom True]]
n2 = [ApplicationNode (LambdaNode [IdentifierAtom "x" 0,IdentifierAtom "y" 0] 
    (IdentifierAtom "z" 0) [ApplicationNode (IdentifierAtom "/" 0) [IdentifierAtom "x" 0,
    IdentifierAtom "y" 0]]) [BoolAtom True,BoolAtom True,BoolAtom True,StringAtom "sdf"]]
n3 = [PairNode (IdentifierAtom "*" 0 ) (PairNode (BoolAtom True) (PairNode (IdentifierAtom "a" 0 ) EmptyAtom))]
n4 =[PairNode (IdentifierAtom "*" 0 ) (PairNode (BoolAtom True) (PairNode (PairNode (BoolAtom True) 
    (PairNode (BoolAtom True) EmptyAtom)) (PairNode (IdentifierAtom "a" 00) EmptyAtom)))]
n5 =[PairNode (IdentifierAtom "*" 0 ) (PairNode (BoolAtom True) (PairNode (ApplicationNode (IdentifierAtom "+" 0) 
    [BoolAtom True,IdentifierAtom "PI" 0]) (PairNode (IdentifierAtom "t" 0 ) EmptyAtom)))]
n6 = [IfNode (IdentifierAtom "p" 0 ) (ApplicationNode (IdentifierAtom "*" 0 ) [BoolAtom True,BoolAtom True]) 
    (PairNode (BoolAtom True) (PairNode (BoolAtom True) EmptyAtom))]
n7 = [SetNode (IdentifierAtom "x" 0) (LambdaNode [] (IdentifierAtom "arglist" 0) 
    [ApplicationNode (IdentifierAtom "length" 0) [IdentifierAtom "arglist" 0]])]
n8 = [PairNode (IdentifierAtom "x" 0) (PairNode (ApplicationNode (IdentifierAtom "*" 0) 
    [BoolAtom True,BoolAtom True]) (PairNode (PairNode (IdentifierAtom "c" 0) 
    (PairNode (BoolAtom True) (PairNode (BoolAtom True) (PairNode 
    (ApplicationNode (IdentifierAtom "+" 0) [BoolAtom True,BoolAtom True]) EmptyAtom)))) 
    EmptyAtom))]
n9 = [LambdaNode [] (IdentifierAtom "X" 0) [ApplicationNode (IdentifierAtom "*" 0) 
    [BoolAtom True,BoolAtom True],SetNode (IdentifierAtom "X" 0) (BoolAtom True),IdentifierAtom "X" 0]]
n10 =[LambdaNode [] (IdentifierAtom "X" 0) [PairNode (BoolAtom True) 
    (PairNode (BoolAtom True) (PairNode (BoolAtom True) EmptyAtom))]]
n11 = [DefineNode (IdentifierAtom "func" 0) (LambdaNode [IdentifierAtom "x" 0,IdentifierAtom "y" 0] 
    (IdentifierAtom "z" 0) [ApplicationNode (IdentifierAtom "+" 0) [ApplicationNode (IdentifierAtom "*" 0)
    [IdentifierAtom "x" 0,ApplicationNode (IdentifierAtom "len" 0) [IdentifierAtom "z" 0]],ApplicationNode 
    (IdentifierAtom "*" 0) [IdentifierAtom "y" 0,ApplicationNode (IdentifierAtom "len" 0) [IdentifierAtom "z" 0]]]])]
n12 = [IfNode (ApplicationNode (IdentifierAtom "Eq" 0) [BoolAtom True,BoolAtom False]) 
    (SetNode (IdentifierAtom "x" 0) (NumberAtom (Exact (Integer 0)))) (IdentifierAtom "" 0)]