module CodeGeneration.CodeGeneration (generate, generateCode) where

import SemanticAnalysis.MetaNode'
import Control.Monad.State.Lazy
import Common.Number
import Data.List
import CodeGeneration.Snippets
import CodeGeneration.BaseFunctions
import qualified Data.Map as M
import Data.Maybe (fromMaybe, fromJust)

newline :: String
newline = "\n"

data Object = Object { code :: String, registered :: Bool } deriving (Show, Eq)

-- lambdas are the only things that need to be 'pulled' to the root level from
-- nested tree elements
-- recursive function returns (ExpressionS somewhat datatype, [ExpressionS/Lambdas])
-- takes a MetaNode' tree and returns the translated MetaNode' (with lambda creations
-- replaced by LAMDBA(n) references) and a list of all LAMBDA(n) definitions within that
-- MetaNode' tree.
-- this is the main function of the code generation and is called on the metanode' that
-- represents the whole program. The fst is the 'main' program, the snd are all lambda
-- definitions of the program.
-- this recursive function must also have an accumulator to create unique lambda and
-- identifier numbers and return marks
-- the recursion happens as soon as a lambda creation is found

-- generate if expressions as ternary operators
-- if it has one branch make SCHEME_UNSPECIFIED the else part
-- eliminates the need for dragging if clauses to the root tree level
-- ! watch out for evaluation guarantees of if in scheme

-- actually ternary operator is unecessary because of the stack in metamorph-clib
-- "dragging" the if to the "root level" should 

-- for a scheme application (a b c ...) evaluate all parameters in order,
-- so that they end up on the stack correctly
-- then apply a so that the result is placed on the stack where a superordinate
-- application might expect it as parameter

-- lambda statements return the last expression

-- set expressions need to have their value evaluated as expression

-- literals are mostly just generated as scheme_new_?() functions that return
-- new dyntypes to register in the stack
-- exeptions are lists, who need to be built with cons and a lot of stack
-- pushes

-- map from baseatom to actual c functions needs to have the correct amount
-- of parameters listed, so that errors in calling the base function are compile
-- time errors in Metamorph and not the C compiler
-- ! this is obsolete, base function parameter errors are now runtime-concerns
-- due to the way c function parameters are effectively bypassed with the stack
-- and incorrect applications aren't C99 errors anymore.3

-- Lambda is changed to PUSH_LITERAL when rendering to String
data Expression = Function Path 
  | Bound Int Int 
  | GlobalBound Int 
  | Return 
  | Applicate Int Path 
  | TailApplicate Int 
  | Push Object 
  | PushLiteral Object 
  | Pop 
  | PopLiteral 
  | Body Int 
  | BodyClose 
  | Lambda Bool Path Int 
  | Continuation Path
  | SetBound Int Int 
  | SetGlobal Int
  | Start Int
  | BaseFunction String Int
  | If
  | Else
  | EndIf
  | Exit deriving (Show, Eq)

type Program = ([Expression], [[Expression]])
data Path = Path [Int] | ResolvedPath Int deriving (Show, Eq, Ord)

generate :: MetaNode' -> String
generate mn =
  preamble ++
  renderedProgram ++
  postscriptum
    where
      renderedProgram = unlines . map renderExpression . resolvePaths $ program
      program = mainE ++ concat functionsE
      (mainE, functionsE) = generateMainCode mn

renderExpression :: Expression -> String
renderExpression = renderExpression'
  where
    renderExpression' (Function (ResolvedPath num)) = "FUNCTION(" ++ show num ++ ")"
    --renderExpression' (Bound parent num) = "BOUND(" ++ show parent ++ "," ++ show num ++ ")"
    renderExpression' (Lambda variadic (ResolvedPath num) pnum) = "PUSH_LITERAL(" ++ (if variadic then "LAMBDA_VARIADIC" else "LAMBDA") ++ "(" ++ show num ++ "," ++ show pnum ++ "))"
    renderExpression' (Continuation (ResolvedPath num)) = "PUSH_LITERAL(CONTINUATION(" ++ show num ++ "))"
    --renderExpression' (GlobalBound num) = "GLOBAL_BOUND(" ++ show num ++ ")"
    renderExpression' (Return) = "RETURN"
    renderExpression' (Applicate pnum (ResolvedPath num)) = "APPLICATE(" ++ show pnum ++ "," ++ show num ++ ")"
    renderExpression' (TailApplicate pnum) = "TAIL_APPLICATE(" ++ show pnum ++ ")"
    renderExpression' (BaseFunction name pnum) = name ++ "(" ++ show pnum ++ ");"
    renderExpression' (Push obj) = "PUSH(" ++ code obj ++ ")"
    renderExpression' (PushLiteral obj) = "PUSH_LITERAL(" ++ code obj ++ ")"
    renderExpression' (Pop) = "POP"
    renderExpression' (PopLiteral) = "POP_LITERAL"
    renderExpression' (Body dnum) = "BODY(" ++ show dnum ++ ")"
    renderExpression' (BodyClose) = "BODY_CLOSE"
    renderExpression' (SetBound parent num) = "SET_BOUND(" ++ show parent ++ "," ++ show num ++ ")"
    renderExpression' (SetGlobal num) = "SET_GLOBAL_BOUND(" ++ show num ++ ")"
    renderExpression' (Start dnum) = "START(" ++ show dnum ++ ")"
    renderExpression' (Exit) = "EXIT"
    renderExpression' (If) = "if (popif()) {"
    renderExpression' (Else) = "} else {"
    renderExpression' (EndIf) = "}"

generateMainCode :: MetaNode'-> Program
generateMainCode (BodyNode' dnum body) = combineProgram ([Start dnum], []) (combineProgram (generateCodeList (Path []) body) ([Exit], []))

generateCode :: Path -> MetaNode'-> Program
-- TODO make DRY
generateCode n (ApplicationNode' _ (BaseFunctionAtom' "call/cc") [expr]) = 
    combinePrograms [
      staticProgram [Continuation n],
      generateCode n expr,
      staticProgram [Applicate 1 n]
    ]
generateCode n (ApplicationNode' _ (BaseFunctionAtom' "call-with-current-continuation") [expr]) = 
    combinePrograms [
      staticProgram [Continuation n],
      generateCode n expr,
      staticProgram [Applicate 1 n]
    ]
generateCode n (ApplicationNode' False expr params) = 
    combinePrograms [
      generateCodeList n (reverse params),
      generateCode (appendPath 0 n) expr,
      staticProgram [Applicate (length params) n]
    ]
generateCode n (ApplicationNode' True expr params) = 
    combinePrograms [
      generateCodeList n (reverse params),
      generateCode (appendPath 0 n) expr,
      staticProgram [TailApplicate (length params)]
    ]
generateCode n (LambdaNode' paramNr variadic (BodyNode' defNr body)) 
  | defNr > 0 = case generateCodeList n body of
    (exprs, lambdas) -> (
        [Lambda variadic n paramNr],
        lambdas ++ [[Function n, Body defNr] ++ exprs ++ [Return, BodyClose]]
      )
  | otherwise = case generateCodeList n body of
    (exprs, lambdas) -> (
        [Lambda variadic n paramNr],
        lambdas ++ [[Function n] ++ exprs ++ [Return]]
      )
generateCode n (SetNode' (BoundAtom' parent num) expr) = 
  combinePrograms [
    generateCode n expr,
    staticProgram [SetBound parent num]
  ]
generateCode n (SetNode' (GlobalAtom' num) expr) = 
  combinePrograms [
    generateCode n expr,
    staticProgram [SetGlobal num]
  ]
generateCode n (IfNode' condition then' else') = 
  combinePrograms [
    generateCode (appendPath 0 n) condition,
    staticProgram [If],
    generateCode (appendPath 1 n) then',
    staticProgram [Else],
    generateCode (appendPath 2 n) else',
    staticProgram [EndIf]
  ]
generateCode n (BodyNode' defNr body) = 
  combinePrograms [
    staticProgram [Body defNr],
    generateCodeList n body,
    staticProgram [Return, BodyClose]
  ]
generateCode n (PairNode' car cdr) = generateCode n (ApplicationNode' False (BaseFunctionAtom' "cons") [car, cdr])
generateCode _ obj = staticProgram [operation (toObject obj)]
  where
    operation = if registered then Push else PushLiteral
    generatedObj@Object{ registered = registered } = toObject obj

generateCodeList :: Path -> [MetaNode'] -> Program
generateCodeList n xs = combinePrograms (zipWith generate xs [1..])
  where generate mn index = generateCode (appendPath index n) mn

emptyProgram :: Program
emptyProgram = ([],[])

staticProgram :: [Expression] -> Program
staticProgram e = (e, [])

combineProgram :: Program -> Program -> Program
combineProgram (ea, la) (eb, lb) = (ea ++ eb, la ++ lb)

combinePrograms :: [Program] -> Program
combinePrograms [] = emptyProgram
combinePrograms xs = foldl1 combineProgram xs

resolvePaths :: [Expression] -> [Expression]
resolvePaths exprs = map resolvePath exprs
  where
    resolvePath (Applicate a path) = Applicate a (fromJust (M.lookup path lookupMap))
    resolvePath (Lambda a path b) = Lambda a (fromJust (M.lookup path lookupMap)) b
    resolvePath (Continuation path) = Continuation (fromJust (M.lookup path lookupMap))
    resolvePath (Function path) = Function (fromJust (M.lookup path lookupMap))
    resolvePath other = other
    lookupMap = fst . foldl folder (M.empty,1) $ exprs
    folder acc@(map, index) expr = if M.member path map then acc else (M.insert path (ResolvedPath index) map, index + 1)
      where path = extractPath expr
    extractPath (Applicate _ path) = path
    extractPath (Lambda _ path _) = path
    extractPath (Continuation path) = path
    extractPath (Function path) = path
    extractPath other = Path []

appendPath :: Int -> Path -> Path
appendPath n (Path xs)= (Path (xs ++ [n]))

toObject :: MetaNode' -> Object
toObject (BoolAtom' True) = Object { code = "scheme_new_boolean(TRUE)", registered = False }
toObject (BoolAtom' False) = Object { code = "scheme_new_boolean(FALSE)", registered = False }
toObject (EmptyAtom') = Object { code = "SCHEME_NULL", registered = False }
toObject (UnspecifiedAtom') = Object { code = "SCHEME_UNSPECIFIED", registered = False }
toObject (StringAtom' str) = Object { code = "scheme_new_string(\"" ++ (str >>= escape) ++ "\")", registered = False }
toObject (BaseFunctionAtom' str) = Object { code = "LAMBDA_BASE(" ++ (baseFunction str) ++ ")", registered = False }
toObject (SymbolAtom' str) = Object { code = "scheme_new_symbol(\"" ++ (str >>= escape) ++ "\")", registered = False }
toObject (NumberAtom' (Exact (Integer int))) = Object { 
    code = "scheme_new_number(scheme_exact_integer(integer_create((char[]) " ++ arrayString ++ ", " ++ show len ++ ")))",
    registered = False 
}
  where
    arrayString = bytesToArrayString bytes
    len = length bytes
    bytes = integerToBytes int
toObject (NumberAtom' (Exact (Rational numerator denominator))) = Object { 
    code = "scheme_new_number(scheme_exact_rational(rational_create_raw((char[]) " ++ arrayStringN ++ ", " ++ show lenN ++ ", (char[])" ++arrayStringD ++ ", " ++ show lenD ++ ")))",
    registered = False 
}
  where
    [arrayStringN, arrayStringD] = map bytesToArrayString bytes
    [lenN, lenD] = map length bytes
    bytes@[bytesN, bytesD] = map integerToBytes [numerator, denominator]
toObject (NumberAtom' (Inexact (Rational numerator denominator))) = Object { code = "scheme_new_number(scheme_inexact_rational(" ++ show (fromInteger numerator / fromInteger denominator) ++ "d))", registered = False }
toObject (NumberAtom' (Inexact (Integer int))) = Object { code = "scheme_new_number(scheme_inexact_integer(" ++ show (fromInteger int :: Double) ++ "d))", registered = False }
toObject (NumberAtom' (Inexact (Real (InfReal double)))) = Object { code = "scheme_new_number(scheme_inexact_real(" ++ show double ++ "d))", registered = False }
toObject (NumberAtom' (Inexact (Real _))) = error "infinities are not supported"
toObject (BoundAtom' parent num) = Object { code = "BOUND(" ++ show parent ++ "," ++ show num ++ ")", registered = True }
toObject (GlobalAtom' num) = Object { code = "GLOBAL_BOUND(" ++ show num ++ ")", registered = True }

integerToArrayString = bytesToArrayString . integerToBytes

bytesToArrayString :: [String] -> String
bytesToArrayString = surround . intercalate ", " . map prefixByte
  where
    surround str = ('{':str) ++ ['}']
    prefixByte x = ('0':'b':x)

integerToBytes :: Integer -> [String]
integerToBytes integer = if signNegative then "00000001":reversedBytes  else "00000000":reversedBytes
  where
    reversedBytes = map (map (head . show)) . reverse . map reverse . groupByte . bin . abs $ integer
    groupByte xs
      | length xs >=8 = take 8 xs : groupByte (drop 8 xs)
      | otherwise = [take 8 (xs ++ repeat 0)]
    signNegative = signum integer < 0

bin :: Integer -> [Integer]
bin 0 = [0]
bin 1 = [1]
bin x = (mod x 2) : (bin (div x 2))

escape '\a' = "\\a"
escape '\b' = "\\b"
escape '\t' = "\\t"
escape '\n' = "\\n"
escape '\r' = "\\r"
escape c = [c]
