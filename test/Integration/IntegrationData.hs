module Integration.IntegrationData where

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