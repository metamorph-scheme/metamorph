module MacroEngine.TemplateMetaNode where
import Common.Number
import Parser.MetaNode

data TemplateMetaNode = TemplateLambdaNode [TemplateMetaNode] TemplateMetaNode [TemplateMetaNode]
            | TemplateAtom MetaNode
            | TemplateIdentifierAtom String 
            | TemplateEllipsisNode TemplateMetaNode
            | TemplateListNode [TemplateMetaNode] | TemplateImproperListNode [TemplateMetaNode] | TemplateIfNode TemplateMetaNode TemplateMetaNode TemplateMetaNode 
            | TemplateSetNode TemplateMetaNode TemplateMetaNode | TemplateDefineNode TemplateMetaNode TemplateMetaNode 
            deriving (Eq, Show)