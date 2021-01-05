module SemanticAnalysis.Testdata where

import Parser.MetaNode
import Common.Number

scipRepresentingTablesAst = [DefineNode (IdentifierAtom "lookup") 
    (LambdaNode [IdentifierAtom "key-1",IdentifierAtom "key-2",IdentifierAtom "table"] (IdentifierAtom "") 
        [ApplicationNode (IdentifierAtom "let") [ApplicationNode 
        (ApplicationNode 
            (IdentifierAtom "subtable") 
                [ApplicationNode (IdentifierAtom "assoc") [IdentifierAtom "key-1",ApplicationNode (IdentifierAtom "cdr") [IdentifierAtom "table"]]]) [],
                IfNode (IdentifierAtom "subtable") 
                    (ApplicationNode (IdentifierAtom "let") [ApplicationNode (ApplicationNode (IdentifierAtom "record") [ApplicationNode (IdentifierAtom "assoc") 
                    [IdentifierAtom "key-2",ApplicationNode (IdentifierAtom "cdr") [IdentifierAtom "subtable"]]]) [],
                    IfNode (IdentifierAtom "record") (ApplicationNode (IdentifierAtom "cdr") [IdentifierAtom "record"]) 
                        (IdentifierAtom "false")]) 
                (IdentifierAtom "false")]])]

multExprAst = 
    [DefineNode (IdentifierAtom "fac") 
        (LambdaNode [IdentifierAtom "n"] (IdentifierAtom "") 
            [IfNode (ApplicationNode (IdentifierAtom ">") [IdentifierAtom "n",NumberAtom (Exact (Integer 0))]) 
                (ApplicationNode (IdentifierAtom "*") 
                    [IdentifierAtom "n",
                    ApplicationNode (IdentifierAtom "fac") [ApplicationNode (IdentifierAtom "-") 
                        [IdentifierAtom "n",NumberAtom (Exact (Integer 1))]]]) 
                    (NumberAtom (Exact (Integer 1)))]),
   DefineNode (IdentifierAtom "lookup") 
    (LambdaNode [IdentifierAtom "key-1",IdentifierAtom "key-2",IdentifierAtom "table"] (IdentifierAtom "") 
        [ApplicationNode (IdentifierAtom "let") [ApplicationNode 
        (ApplicationNode 
            (IdentifierAtom "subtable") 
                [ApplicationNode (IdentifierAtom "assoc") [IdentifierAtom "key-1",ApplicationNode (IdentifierAtom "cdr") [IdentifierAtom "table"]]]) [],
                IfNode (IdentifierAtom "subtable") 
                    (ApplicationNode (IdentifierAtom "let") [ApplicationNode (ApplicationNode (IdentifierAtom "record") [ApplicationNode (IdentifierAtom "assoc") 
                    [IdentifierAtom "key-2",ApplicationNode (IdentifierAtom "cdr") [IdentifierAtom "subtable"]]]) [],
                    IfNode (IdentifierAtom "record") (ApplicationNode (IdentifierAtom "cdr") [IdentifierAtom "record"]) 
                        (IdentifierAtom "false")]) 
                (IdentifierAtom "false")]]),
    ApplicationNode (IdentifierAtom "fac") [NumberAtom (Exact (Integer 4))]]


n1 = [ApplicationNode (IdentifierAtom "+") [BoolAtom True,BoolAtom True]]
n2 = [ApplicationNode (LambdaNode [IdentifierAtom "x",IdentifierAtom "y"] 
    (IdentifierAtom "z") [ApplicationNode (IdentifierAtom "/") [IdentifierAtom "x",
    IdentifierAtom "y"]]) [BoolAtom True,BoolAtom True,BoolAtom True,StringAtom "sdf"]]
n3 = [PairNode (IdentifierAtom "*") (PairNode (BoolAtom True) (PairNode (IdentifierAtom "a") EmptyAtom))]
n4 =[PairNode (IdentifierAtom "*") (PairNode (BoolAtom True) (PairNode (PairNode (BoolAtom True) 
    (PairNode (BoolAtom True) EmptyAtom)) (PairNode (IdentifierAtom "a") EmptyAtom)))]
n5 =[PairNode (IdentifierAtom "*") (PairNode (BoolAtom True) (PairNode (ApplicationNode (IdentifierAtom "+") 
    [BoolAtom True,IdentifierAtom "PI"]) (PairNode (IdentifierAtom "t") EmptyAtom)))]
n6 = [IfNode (IdentifierAtom "p") (ApplicationNode (IdentifierAtom "*") [BoolAtom True,BoolAtom True]) 
    (PairNode (BoolAtom True) (PairNode (BoolAtom True) EmptyAtom))]
n7 = [SetNode (IdentifierAtom "x") (LambdaNode [] (IdentifierAtom "arglist") 
    [ApplicationNode (IdentifierAtom "length") [IdentifierAtom "arglist"]])]
n8 = [PairNode (IdentifierAtom "x") (PairNode (ApplicationNode (IdentifierAtom "*") 
    [BoolAtom True,BoolAtom True]) (PairNode (PairNode (IdentifierAtom "c") 
    (PairNode (BoolAtom True) (PairNode (BoolAtom True) (PairNode 
    (ApplicationNode (IdentifierAtom "+") [BoolAtom True,BoolAtom True]) EmptyAtom)))) 
    EmptyAtom))]
n9 = [LambdaNode [] (IdentifierAtom "X") [ApplicationNode (IdentifierAtom "*") 
    [BoolAtom True,BoolAtom True],SetNode (IdentifierAtom "X") (BoolAtom True),IdentifierAtom "X"]]
n10 =[LambdaNode [] (IdentifierAtom "X") [PairNode (BoolAtom True) 
    (PairNode (BoolAtom True) (PairNode (BoolAtom True) EmptyAtom))]]
n11 = [DefineNode (IdentifierAtom "func") (LambdaNode [IdentifierAtom "x",IdentifierAtom "y"] 
    (IdentifierAtom "z") [ApplicationNode (IdentifierAtom "+") [ApplicationNode (IdentifierAtom "*")
    [IdentifierAtom "x",ApplicationNode (IdentifierAtom "len") [IdentifierAtom "z"]],ApplicationNode 
    (IdentifierAtom "*") [IdentifierAtom "y",ApplicationNode (IdentifierAtom "len") [IdentifierAtom "z"]]]])]