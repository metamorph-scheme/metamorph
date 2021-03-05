module MacroEngine.TemplateMetaNode where
import Common.Number
import Parser.MetaNode

data TemplateMetaNode = TemplateLambdaNode [TemplateMetaNode] TemplateMetaNode [TemplateMetaNode]
            | TemplateAtom MetaNode
            | TemplateIdentifierAtom [Integer] String Int
            | TemplateEllipsisNode Integer [Integer] TemplateMetaNode
            | TemplateListNode [TemplateMetaNode] 
            | TemplateImproperListNode [TemplateMetaNode] 
            | TemplateIfNode TemplateMetaNode TemplateMetaNode TemplateMetaNode 
            | TemplateSetNode TemplateMetaNode TemplateMetaNode 
            | TemplateDefineNode TemplateMetaNode TemplateMetaNode 
            | TemplateLetSyntaxNode TemplateMetaNode [TemplateMetaNode]
            | TemplateLetrecSyntaxNode TemplateMetaNode [TemplateMetaNode]
            | TemplateDefineSyntaxNode TemplateMetaNode TemplateMetaNode
            deriving (Eq, Show)