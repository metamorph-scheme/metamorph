module Integration.IntegrationData where

import Parser.MetaNode
import Common.Number

scipRepresentingTablesAst = [DefineNode (IdentifierAtom "lookup" 0) (LambdaNode [IdentifierAtom "key-1" 0,IdentifierAtom "key-2" 0,IdentifierAtom "table" 0] (IdentifierAtom "" 0) [ApplicationNode (IdentifierAtom "let" 0) [ApplicationNode (ApplicationNode (IdentifierAtom "subtable" 0) [ApplicationNode (IdentifierAtom "assoc" 0) [IdentifierAtom "key-1" 0,ApplicationNode (IdentifierAtom "cdr" 0) [IdentifierAtom "table" 0]]]) [],IfNode (IdentifierAtom "subtable" 0) (ApplicationNode (IdentifierAtom "let" 0) [ApplicationNode (ApplicationNode (IdentifierAtom "record" 0) [ApplicationNode (IdentifierAtom "assoc" 0) [IdentifierAtom "key-2" 0,ApplicationNode (IdentifierAtom "cdr" 0) [IdentifierAtom "subtable" 0]]]) [],IfNode (IdentifierAtom "record" 0) (ApplicationNode (IdentifierAtom "cdr" 0) [IdentifierAtom "record" 0]) (BoolAtom False)]) (BoolAtom False)]])]

multExprAst = 
    [DefineNode (IdentifierAtom "fac" 0) 
        (LambdaNode [IdentifierAtom "n" 0] (IdentifierAtom "" 0) 
            [IfNode (ApplicationNode (IdentifierAtom ">" 0) [IdentifierAtom "n" 0,NumberAtom (Exact (Integer 0))]) 
                (ApplicationNode (IdentifierAtom "*" 0) 
                    [IdentifierAtom "n" 0,
                    ApplicationNode (IdentifierAtom "fac" 0) [ApplicationNode (IdentifierAtom "-" 0) 
                        [IdentifierAtom "n" 0,NumberAtom (Exact (Integer 1))]]]) 
                    (NumberAtom (Exact (Integer 1)))]),
   DefineNode (IdentifierAtom "lookup" 0) 
    (LambdaNode [IdentifierAtom "key-1" 0,IdentifierAtom "key-2" 0,IdentifierAtom "table" 0] (IdentifierAtom "" 0) 
        [ApplicationNode (IdentifierAtom "let" 0) [ApplicationNode 
        (ApplicationNode 
            (IdentifierAtom "subtable" 0) 
                [ApplicationNode (IdentifierAtom "assoc" 0) [IdentifierAtom "key-1" 0,ApplicationNode (IdentifierAtom "cdr" 0) [IdentifierAtom "table" 0]]]) [],
                IfNode (IdentifierAtom "subtable" 0) 
                    (ApplicationNode (IdentifierAtom "let" 0) [ApplicationNode (ApplicationNode (IdentifierAtom "record" 0) [ApplicationNode (IdentifierAtom "assoc" 0) 
                    [IdentifierAtom "key-2" 0,ApplicationNode (IdentifierAtom "cdr" 0) [IdentifierAtom "subtable" 0]]]) [],
                    IfNode (IdentifierAtom "record" 0) (ApplicationNode (IdentifierAtom "cdr" 0) [IdentifierAtom "record" 0]) 
                        (IdentifierAtom "false" 0)]) 
                (IdentifierAtom "false" 0)]]),
    ApplicationNode (IdentifierAtom "fac" 0) [NumberAtom (Exact (Integer 4))]]