module SemanticAnalysis.MetaNode' where

import Common.Number
import Parser.MetaNode


schemeFunctions :: [String]
schemeFunctions = ["*","+","-","...","/","<","<=","=","=>",">",">=","abs","append","apply","assoc","assq","assv","binary-port?","boolean=?","boolean?","bytevector","bytevector-append","bytevector-copy","bytevector-copy!","bytevector-length","bytevector-u8-ref","bytevector-u8-set!","bytevector?","caar","cadr","call-with-current-continuation","call-with-port","call-with-values","call/cc","car","cdar","cddr","cdr","ceiling","char->integer","char-ready?","char<=?","char<?","char=?","char>=?","char>?","char?","close-input-port","close-output-port","close-port","complex?","cons","current-error-port","current-input-port","current-output-port","define","define-record-type","define-syntax","define-values","denominator","dynamic-wind","74","Revised7","Scheme","else","eof-object","eof-object?","eq?","equal?","eqv?","error","error-object-irritants","error-object-message","error-object?","even?","exact","exact-integer-sqrt","exact-integer?","exact?","expt","features","file-error?","floor","floor-quotient","floor-remainder","floor/","flush-output-port","for-each","gcd","get-output-bytevector","get-output-string","if","include","include-ci","inexact","inexact?","input-port-open?","input-port?","integer->char","integer?","lambda","lcm","length","let-syntax","letrec-syntax","list","list->string","list->vector","list-copy","list-ref","list-set!","list-tail","list?","make-bytevector","make-list","make-string","make-vector","map","max","member","memq","memv","min","modulo","negative?","newline","not","null?","number->string","number?","numerator","odd?","open-input-bytevector","open-input-string","open-output-bytevector","open-output-string","output-port-open?","output-port?","pair?","peek-char","peek-u8","port?","positive?","procedure?","quasiquote","quote","quotient","raise","raise-continuable","rational?","rationalize","read-bytevector","read-bytevector!","read-char","read-error?","read-line","read-string","read-u8","real?","remainder","reverse","round","set!","set-car!","set-cdr!","square","string","string->list","string->number","string->symbol","string->utf8","string->vector","string-append","string-copy","string-copy!","string-fill!","string-for-each","string-length","string-map","string-ref","string-set!","string<=?","string<?","string=?","string>=?","string>?","string?","substring","symbol->string","symbol=?","symbol?","syntax-error","syntax-rules","textual-port?","truncate","truncate-quotient","truncate-remainder","truncate/","u8-ready?","unquote","unquote-splicing","utf8->string","values","vector","vector->list","vector->string","vector-append","vector-copy","vector-copy!","vector-fill!","vector-for-each","vector-length","vector-map","vector-ref","vector-set!","vector?","with-exception-handler","write-bytevector","write-char","write-string","write-u8","zero"]

schemeMakros :: [String]
schemeMakros = ["guard","parameterize","make-parameter","do","begin","and","case","cond","cond-expand","let","let*","let-values","let*-values","letrec","letrec*","or","unless","when"]

data MetaNode' = BodyNode' Int [MetaNode'] -- Number of Bodyparams, only generate BODY directive if > 0
    -- Formal number of params, additional variadic param
    -- construction of variadic at runtime in lambda! handled by C Lib
    | LambdaNode' Int Bool MetaNode'
    | PairNode' MetaNode' MetaNode' 
    | NumberAtom' Number 
    | EmptyAtom' 
    | StringAtom' String 
    | BoolAtom' Bool 
    | CharAtom' Char
    | BaseFunctionAtom' String
    | BaseSyntaxAtom' String Int
    | SyntaxAtom' (MetaNode->MetaNode) Int
    | BoundAtom' Int Int -- Not literal
    | GlobalAtom' Int -- Not literal
    -- Distinction between application of function or continuation needs to be handled at runtime! handled by C Lib
    | ApplicationNode' Bool MetaNode' [MetaNode'] -- Tailcall Mark; implicit number of real params
    | IfNode' MetaNode' MetaNode' MetaNode' -- In order to become expression, stack is needed
    | SetNode' MetaNode' MetaNode' -- defines become set

  
data SymbolTable = Activation [(String, Int)] SymbolTable
    | Syntax [(String, MetaNode -> MetaNode)]  SymbolTable
    | Scope Int SymbolTable
    | Global

instance Show SymbolTable where
    show (Activation ls tb) = "Actvation: " ++ show ls ++ "\n" ++ show tb
    show (Syntax ls tb) = "Syntax: " ++ show (fst <$> ls) ++ "\n" ++ show tb
    show Global = "Global"

instance Show MetaNode' where
    show (LambdaNode' n False mns') = "Lambda(" ++ show n ++ "):\n" ++ show mns'
    show (BodyNode' n mns') = "Body(" ++ show n ++ "): " ++ show mns'
    show (LambdaNode' n True mns') = "LambdaVariadic(" ++ show n ++ " + 1):\n" ++ show mns' 
    show (PairNode' mn' mn2') = "Pair: " ++ show (mn',mn2') 
    show (NumberAtom' n) = show n
    show (EmptyAtom') = "Empty"
    show (StringAtom' str) = show str
    show (BoolAtom' b) = show b
    show (CharAtom' c) = show c
    show (BaseFunctionAtom' str) = "BaseFunction: " ++ show str 
    show (BaseSyntaxAtom' str _) = "BaseSyntax: " ++ show str
    show (SyntaxAtom' _ _) = "Syntax" 
    show (BoundAtom' n m) = "Bound" ++ show (n,m)
    show (GlobalAtom' n) = "Global(" ++ show n ++ ")"
    show (ApplicationNode' False mn' mns') = "Application(" ++ show mn' ++ "): \n"++ show mns'
    show (ApplicationNode' True mn' mns') = "TailApplication(" ++ show mn' ++ "): \n"++ show mns'
    show (IfNode' mn' mn2' mn3') = "If \n" ++ show mn' ++ "\nThen \n"++ show mn2' ++ "\nElse \n" ++ show mn3'
    show (SetNode' mn' mn2')  = "Set(" ++ show mn' ++ "): " ++ show mn2'