module Parser.MetaNode where
import Common.Number

data MetaNode = LambdaNode [MetaNode] MetaNode [MetaNode]
    | PairNode MetaNode MetaNode 
    | NumberAtom Number 
    | UnspecifiedAtom
    | EmptyAtom 
    | BoolAtom Bool 
    | StringAtom String 
    | SymbolAtom String 
    | CharAtom Char 
    | IdentifierAtom String Int -- Becomes Number Pair or Internal Enum
    | ApplicationNode MetaNode [MetaNode] 
    | IfNode MetaNode MetaNode MetaNode 
    | SetNode MetaNode MetaNode 
    | DefineNode MetaNode MetaNode -- Toplevel Defines will be counted
    | LetSyntaxNode MetaNode [MetaNode] 
    | DefineSyntaxNode MetaNode MetaNode
    | LetrecSyntaxNode MetaNode [MetaNode] 
    deriving (Eq, Show)