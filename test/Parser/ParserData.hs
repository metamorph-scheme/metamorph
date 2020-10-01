module Parser.ParserData where
import Lexer.Token
import Parser.MetaNode

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


n1 = ApplicationNode (IdentifierAtom "+") [BoolAtom True,BoolAtom True]
n2 = ApplicationNode (LambdaNode [IdentifierAtom "x",IdentifierAtom "y"] 
    (IdentifierAtom "z") [(ApplicationNode (IdentifierAtom "/") [IdentifierAtom "x",
    IdentifierAtom "y"])]) [BoolAtom True,BoolAtom True,BoolAtom True,StringAtom "sdf"] 
n3 = PairNode (IdentifierAtom "*") (PairNode (BoolAtom True) (PairNode (IdentifierAtom "a") EmptyAtom))
n4 =PairNode (IdentifierAtom "*") (PairNode (BoolAtom True) (PairNode (PairNode (BoolAtom True) 
    (PairNode (BoolAtom True) EmptyAtom)) (PairNode (IdentifierAtom "a") EmptyAtom)))
n5 =PairNode (IdentifierAtom "*") (PairNode (BoolAtom True) (PairNode (ApplicationNode (IdentifierAtom "+") 
    [BoolAtom True,IdentifierAtom "PI"]) (PairNode (IdentifierAtom "t") EmptyAtom)))
n6 = IfNode (IdentifierAtom "p") (ApplicationNode (IdentifierAtom "*") [BoolAtom True,BoolAtom True]) 
    (PairNode (BoolAtom True) (PairNode (BoolAtom True) EmptyAtom))
n7 = SetNode (IdentifierAtom "x") (LambdaNode [] (IdentifierAtom "arglist") 
    [(ApplicationNode (IdentifierAtom "length") [IdentifierAtom "arglist"])])
n8 = PairNode (IdentifierAtom "x") (PairNode (ApplicationNode (IdentifierAtom "*") 
    [BoolAtom True,BoolAtom True]) (PairNode (PairNode (IdentifierAtom "c") 
    (PairNode (BoolAtom True) (PairNode (BoolAtom True) (PairNode 
    (ApplicationNode (IdentifierAtom "+") [BoolAtom True,BoolAtom True]) EmptyAtom)))) 
    EmptyAtom))
n9 = LambdaNode [] (IdentifierAtom "X") [ApplicationNode (IdentifierAtom "*") 
    [BoolAtom True,BoolAtom True],SetNode (IdentifierAtom "X") (BoolAtom True),IdentifierAtom "X"]
n10 =LambdaNode [] (IdentifierAtom "X") [PairNode (BoolAtom True) 
    (PairNode (BoolAtom True) (PairNode (BoolAtom True) EmptyAtom))]
n11 = DefineNode (IdentifierAtom "func") (LambdaNode [IdentifierAtom "x",IdentifierAtom "y"] 
    (IdentifierAtom "z") [ApplicationNode (IdentifierAtom "+") [ApplicationNode (IdentifierAtom "*")
    [IdentifierAtom "x",ApplicationNode (IdentifierAtom "len") [IdentifierAtom "z"]],ApplicationNode 
    (IdentifierAtom "*") [IdentifierAtom "y",ApplicationNode (IdentifierAtom "len") [IdentifierAtom "z"]]]])