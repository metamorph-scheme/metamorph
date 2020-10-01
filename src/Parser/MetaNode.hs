module Parser.MetaNode where
import Common.Number

data MetaNode = LambdaNode [MetaNode] MetaNode [MetaNode] | PairNode MetaNode MetaNode 
            | NumberAtom Number | EmptyAtom | StringAtom String 
            | BoolAtom Bool | CharAtom Char | IdentifierAtom String 
            | ApplicationNode MetaNode [MetaNode] | IfNode MetaNode MetaNode MetaNode 
            | SetNode MetaNode MetaNode | DefineNode MetaNode MetaNode 
            deriving (Eq, Show)