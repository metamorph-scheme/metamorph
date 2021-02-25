module CodeGeneration.CodeGeneration where

import SemanticAnalysis.MetaNode'
import Control.Monad.State.Lazy

newline :: String
newline = "\n"

data Object = Object { code :: String, registered :: Bool } | 

firstPass :: MetaNode' -> MetaNode' -> [String]

toObject :: MetaNode' -> Object
generateCode (BoolAtom' True) = Object { code = "scheme_new_boolean(TRUE)", registered = False }
generateCode (BoolAtom' False) = Object { code = "scheme_new_boolean(FALSE)", registered = False }
generateCode (EmptyAtom') = Object { code = "SCHEME_NULL", registered = False }

-- (<start block>, <lambdas>)
generate :: MetaNode' -> (String, [String])
generate (PairNode' a b) = (generateCode a ++ "\n" ++ generateCode b)
  where code_a = generateCode a

-- without lambdas
generateCode :: MetaNode' -> String
generateCode (BoolAtom' True) = "scheme_new_boolean(TRUE)"
generateCode (BoolAtom' False) = "scheme_new_boolean(False)"


generateCode (PairNode' a b) = "scheme_new_boolean(False)"