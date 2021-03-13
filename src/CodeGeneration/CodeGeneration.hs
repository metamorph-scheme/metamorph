module CodeGeneration.CodeGeneration (generate) where

import SemanticAnalysis.MetaNode'
import Control.Monad.State.Lazy
import Common.Number
import Data.List
import CodeGeneration.Snippets

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
-- and incorrect applications aren't C99 errors anymore.


-- Lambda is changed to PUSH_LITERAL when rendering to String
data Expression = Function Integer 
  | Bound Integer Integer 
  | GlobalBound Integer 
  | Return 
  | Applicate Integer Integer 
  | TailApplicate Integer 
  | Push Object 
  | PushLiteral Object 
  | Pop 
  | PopLiteral 
  | Body Integer 
  | BodyClose 
  | Lambda Integer Integer 
  | SetBound Integer Integer 
  | SetGlobal Integer
  | Start Integer
  | Exit deriving (Show, Eq)

generate :: MetaNode' -> String
generate mn =
  preamble ++
  main ++
  functions
    where
      main = unlines . map renderExpression $ mainE
      functions = unlines . concat . map (map renderExpression) $ functionsE
      (mainE, functionsE) = generateMainCode mn

renderExpression :: Expression -> String
renderExpression = renderExpression'
  where
    renderExpression' (Function num) = "FUNCTION(" ++ show num ++ ")"
    renderExpression' (Bound parent num) = "BOUND(" ++ show parent ++ "," ++ show num ++ ")"
    renderExpression' (Lambda pnum num) = "LAMBDA(" ++ show pnum ++ "," ++ show num ++ ")"
    renderExpression' (GlobalBound num) = "GLOBAL(" ++ show num ++ ")"
    renderExpression' (Return) = "RETURN"
    renderExpression' (Applicate pnum fnum) = "APPLICATE(" ++ show pnum ++ "," ++ show fnum ++ ")"
    renderExpression' (TailApplicate fnum) = "TAIL_APPLICATE(" ++ show fnum ++ ")"
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

generateMainCode :: MetaNode'-> ([Expression], [[Expression]])
generateMainCode (BodyNode' dnum body) = combineCode ([Start (toInteger dnum)], []) (combineCode (generateCodeList [] body) ([Exit], []))

generateCode :: [Integer] -> MetaNode'-> ([Expression], [[Expression]])
generateCode n (ApplicationNode' False expr params) = 
    combineCode (generateCodeList n params) (generateCode (n++[0]) expr)
-- TODO variadic flag?
generateCode n (LambdaNode' paramNr variadic (BodyNode' defNr body)) = case generateCodeList n body of
  (exprs, lambdas) -> ([Lambda (toInteger paramNr) (renderPath n)], lambdas ++ [[Function (renderPath n), Body (toInteger defNr)] ++ exprs ++ [Return, BodyClose]])
generateCode n (SetNode' (BoundAtom' parent num) expr) = combineCode (generateCode n expr) ([SetBound (toInteger parent) (toInteger num)], [])
generateCode n (SetNode' (GlobalAtom' num) expr) = combineCode (generateCode n expr) ([SetGlobal (toInteger num)], [])
generateCode n (BodyNode' defNr body) = combineCode (combineCode ([Body (toInteger defNr)], []) (generateCodeList n body)) ([Return, BodyClose], [])
generateCode _ obj = ([operation (toObject obj)], [])
  where
    operation = if registered then Push else PushLiteral
    generatedObj@Object{ registered = registered } = toObject obj

generateCodeList :: [Integer] -> [MetaNode'] -> ([Expression], [[Expression]])
generateCodeList n xs = foldl1 combineCode (zipWith generate xs [1..])
  where generate mn index = generateCode (n ++ [index]) mn

combineCode :: ([Expression], [[Expression]]) -> ([Expression], [[Expression]]) -> ([Expression], [[Expression]])
combineCode (ea, la) (eb, lb) = (ea ++ eb, la ++ lb)

renderPath :: [Integer] -> Integer
renderPath = read . concat . map show

toObject :: MetaNode' -> Object
toObject (BoolAtom' True) = Object { code = "scheme_new_boolean(TRUE)", registered = False }
toObject (BoolAtom' False) = Object { code = "scheme_new_boolean(FALSE)", registered = False }
toObject (EmptyAtom') = Object { code = "SCHEME_NULL", registered = False }
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
toObject (GlobalAtom' num) = Object { code = "GLOBAL(" ++ show num ++ ")", registered = True }

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

-- (<start block>, <lambdas>)
-- generate :: MetaNode' -> (String, [String])
-- generate (PairNode' a b) = (generateCode a ++ newline ++ generateCode b)
--   where code_a = generateCode a

-- -- without lambdas
-- generateCode :: MetaNode' -> String
-- generateCode (BoolAtom' True) = "scheme_new_boolean(TRUE)"
-- generateCode (BoolAtom' False) = "scheme_new_boolean(False)"


-- generateCode (PairNode' a b) = "scheme_new_boolean(False)"