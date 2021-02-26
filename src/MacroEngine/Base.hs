module MacroEngine.Base ( baseSyntax ) where

import qualified Data.Map as M
import Parser.MetaNode

baseSyntax :: M.Map String MetaNode
baseSyntax =
  M.fromList [
    ("let", letMacro),
    ("let*", letStarMacro),
    ("letrec", letrecMacro),
    ("and", andMacro),
    ("or", orMacro)
  ]

letMacro :: MetaNode
letMacro = ApplicationNode (IdentifierAtom "syntax-rules") [EmptyAtom,ApplicationNode (ApplicationNode (IdentifierAtom "let") [ApplicationNode (ApplicationNode (IdentifierAtom "name") [IdentifierAtom "val"]) [IdentifierAtom "..."],IdentifierAtom "body1",IdentifierAtom "body2",IdentifierAtom "..."]) [ApplicationNode (LambdaNode [IdentifierAtom "name",IdentifierAtom "..."] (IdentifierAtom "") [IdentifierAtom "body1",IdentifierAtom "body2",IdentifierAtom "..."]) [IdentifierAtom "val",IdentifierAtom "..."]],ApplicationNode (ApplicationNode (IdentifierAtom "let") [IdentifierAtom "tag",ApplicationNode (ApplicationNode (IdentifierAtom "name") [IdentifierAtom "val"]) [IdentifierAtom "..."],IdentifierAtom "body1",IdentifierAtom "body2",IdentifierAtom "..."]) [ApplicationNode (ApplicationNode (IdentifierAtom "letrec") [ApplicationNode (ApplicationNode (IdentifierAtom "tag") [LambdaNode [IdentifierAtom "name",IdentifierAtom "..."] (IdentifierAtom "") [IdentifierAtom "body1",IdentifierAtom "body2",IdentifierAtom "..."]]) [],IdentifierAtom "tag"]) [IdentifierAtom "val",IdentifierAtom "..."]]]

letStarMacro :: MetaNode
letStarMacro = ApplicationNode (IdentifierAtom "syntax-rules") [EmptyAtom,ApplicationNode (ApplicationNode (IdentifierAtom "let*") [EmptyAtom,IdentifierAtom "body1",IdentifierAtom "body2",IdentifierAtom "..."]) [ApplicationNode (IdentifierAtom "let") [EmptyAtom,IdentifierAtom "body1",IdentifierAtom "body2",IdentifierAtom "..."]],ApplicationNode (ApplicationNode (IdentifierAtom "let*") [ApplicationNode (ApplicationNode (IdentifierAtom "name1") [IdentifierAtom "val1"]) [ApplicationNode (IdentifierAtom "name2") [IdentifierAtom "val2"],IdentifierAtom "..."],IdentifierAtom "body1",IdentifierAtom "body2",IdentifierAtom "..."]) [ApplicationNode (IdentifierAtom "let") [ApplicationNode (ApplicationNode (IdentifierAtom "name1") [IdentifierAtom "val1"]) [],ApplicationNode (IdentifierAtom "let*") [ApplicationNode (ApplicationNode (IdentifierAtom "name2") [IdentifierAtom "val2"]) [IdentifierAtom "..."],IdentifierAtom "body1",IdentifierAtom "body2",IdentifierAtom "..."]]]]

letrecMacro :: MetaNode
letrecMacro = ApplicationNode (IdentifierAtom "syntax-rules") [EmptyAtom,ApplicationNode (ApplicationNode (IdentifierAtom "letrec") [ApplicationNode (ApplicationNode (IdentifierAtom "var1") [IdentifierAtom "init1"]) [IdentifierAtom "..."],IdentifierAtom "body",IdentifierAtom "..."]) [ApplicationNode (IdentifierAtom "letrec") [StringAtom "generatetempnames",ApplicationNode (IdentifierAtom "var1") [IdentifierAtom "..."],EmptyAtom,ApplicationNode (ApplicationNode (IdentifierAtom "var1") [IdentifierAtom "init1"]) [IdentifierAtom "..."],IdentifierAtom "body",IdentifierAtom "..."]],ApplicationNode (ApplicationNode (IdentifierAtom "letrec") [StringAtom "generatetempnames",EmptyAtom,ApplicationNode (IdentifierAtom "temp1") [IdentifierAtom "..."],ApplicationNode (ApplicationNode (IdentifierAtom "var1") [IdentifierAtom "init1"]) [IdentifierAtom "..."],IdentifierAtom "body",IdentifierAtom "..."]) [ApplicationNode (IdentifierAtom "let") [ApplicationNode (ApplicationNode (IdentifierAtom "var1") [IdentifierAtom "<undefined>"]) [IdentifierAtom "..."],ApplicationNode (IdentifierAtom "let") [ApplicationNode (ApplicationNode (IdentifierAtom "temp1") [IdentifierAtom "init1"]) [IdentifierAtom "..."],SetNode (IdentifierAtom "var1") (IdentifierAtom "temp1"),IdentifierAtom "...body",IdentifierAtom "..."]]],ApplicationNode (ApplicationNode (IdentifierAtom "letrec") [StringAtom "generatetempnames",ApplicationNode (IdentifierAtom "x") [IdentifierAtom "y",IdentifierAtom "..."],ApplicationNode (IdentifierAtom "temp") [IdentifierAtom "..."],ApplicationNode (ApplicationNode (IdentifierAtom "var1") [IdentifierAtom "init1"]) [IdentifierAtom "..."],IdentifierAtom "body",IdentifierAtom "..."]) [ApplicationNode (IdentifierAtom "letrec") [StringAtom "generatetempnames",ApplicationNode (IdentifierAtom "y") [IdentifierAtom "..."],ApplicationNode (IdentifierAtom "newtemp") [IdentifierAtom "temp",IdentifierAtom "..."],ApplicationNode (ApplicationNode (IdentifierAtom "var1") [IdentifierAtom "init1"]) [IdentifierAtom "..."],IdentifierAtom "body",IdentifierAtom "..."]]]

andMacro :: MetaNode
andMacro = ApplicationNode (IdentifierAtom "syntax-rules") [EmptyAtom,ApplicationNode (ApplicationNode (IdentifierAtom "and") []) [BoolAtom True],ApplicationNode (ApplicationNode (IdentifierAtom "and") [IdentifierAtom "test"]) [IdentifierAtom "test"],ApplicationNode (ApplicationNode (IdentifierAtom "and") [IdentifierAtom "test1",IdentifierAtom "test2",IdentifierAtom "..."]) [IfNode (IdentifierAtom "test1") (ApplicationNode (IdentifierAtom "and") [IdentifierAtom "test2",IdentifierAtom "..."]) (BoolAtom False)]]

orMacro :: MetaNode
orMacro = ApplicationNode (IdentifierAtom "syntax-rules") [EmptyAtom,ApplicationNode (ApplicationNode (IdentifierAtom "and") []) [BoolAtom True],ApplicationNode (ApplicationNode (IdentifierAtom "and") [IdentifierAtom "test"]) [IdentifierAtom "test"],ApplicationNode (ApplicationNode (IdentifierAtom "and") [IdentifierAtom "test1",IdentifierAtom "test2",IdentifierAtom "..."]) [IfNode (IdentifierAtom "test1") (ApplicationNode (IdentifierAtom "and") [IdentifierAtom "test2",IdentifierAtom "..."]) (BoolAtom False)]]
