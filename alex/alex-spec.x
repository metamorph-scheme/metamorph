{
module Lexer.Lexer (scan) where

import Lexer.Token
import qualified Data.Stack as Stack
}

%wrapper "monadUserState"

-- character sets

$digit = [0-9]
$letter = [a-zA-Z]
$specialInitial = [!\$\%&\*\/:\<\=\>\?\^\~]
$mnemonicEscape = [\\a\\b\\t\\n\\r]
$dot = \.
$hexDigit = [0-9a-f]
$verticalLine = \|
$intralineWhitespace = [\ \t]
$openingBracket = \(
$closingBracket = \)
$explicitSign = [\+\-]

-- general

@lineEnding = (\n | \r\n | \r)
@whitespace = ($intralineWhitespace | @lineEnding)
@directive = (\#!fold-case | \#!no-fold-case)
@characterName = (alarm|backspace|delete|escape|newline|null|return|space|tab)
@comment = \;.*
@atmosphere = (@whitespace | @comment | @directive)
@intertokenSpace = @atmosphere*
@delimiter = (@whitespace
             | $verticalLine
             | \(
             | \)
             | \"
             | \;)
@datumComment = \#\;
@openingBlockComment = \#\|
@closingBlockComment = \|\#

-- identifier

@initial = ($letter | $specialInitial)
@specialSubsequent = ($explicitSign | $dot | @)
@subsequent = (@initial | $digit | @specialSubsequent)
@hexScalarValue = $hexDigit+
@inlineHexExcape = \\x(@hexScalarValue)\;
@symbolElement = ([^$verticalLine\\] | @inlineHexExcape | $mnemonicEscape | \\\|)
@signSubsequent = (@initial | $explicitSign | @)
@dotSubsequent = (@signSubsequent | $dot)
@peculiarIdentifier = ($explicitSign
                      | $explicitSign @signSubsequent @subsequent*
                      | $explicitSign $dot @dotSubsequent @subsequent*
                      | $dot @dotSubsequent @subsequent*)
@identifier = (@initial @subsequent*
              | $verticalLine @symbolElement+ $verticalLine
              | @peculiarIdentifier)

-- primitive expressions

@lambda = lambda
@set = set!
@if = if

-- number

$digit2 = [01]
$digit8 = [0-7]
$digit10 = $digit
$digit16 = [$digit10 a-f]

@radix2 = \#b
@radix8 = \#o
@radix10 = (\#d)?
@radix16 = \#x

@exactness = (\#i|\#e)?
@sign = (\+|\-)?
@exponentMarker = e
@suffix = (@exponentMarker @sign $digit10+)?

@infnan = (\+inf.0 | \-inf.0 | \+nan.0 | \-nan.0)

@prefix2 = (@radix2 @exactness | @exactness @radix2)
@prefix8 = (@radix8 @exactness | @exactness @radix8)
@prefix10 = (@radix10 @exactness | @exactness @radix10)
@prefix16 = (@radix16 @exactness | @exactness @radix16)

@uinteger10 = @prefix10 @sign $digit10+

@decimal10 = @prefix10 @sign (@uinteger10 @suffix | $dot $digit10+ @suffix | $digit10+ $dot $digit10* @suffix)

@ureal10 = @prefix10 @sign (@uinteger10 | @uinteger10 \/ @uinteger10 | @decimal10)

@real10 = @prefix10 (@sign @ureal10 | @infnan)

@complex10 = @prefix10 ( @real10
             | @real10 @ @real10 
             | @real10 \+ @real10 i
             | @real10 \- @real10 i
             | @real10 \+ i
             | @real10 \- i
             | @real10 @infnan i
             | \+ @ureal10 i
             | \- @ureal10 i
             | @infnan i
             | \+ i
             | \- i)


tokens :-

<0> {
  @whitespace+				;      -- ignore whitespace
	@comment 						;
  @openingBlockComment      { pushStack nestedComment }

  $openingBracket           { simpleToken POpen }  
  $closingBracket           { simpleToken PClose }
  @lambda                   { simpleToken Lambda }
  @set                      { simpleToken Set }
  @if                       { simpleToken If }

  @uinteger10               { stringToken (\s -> Integral (read s :: Int)) }
  @decimal10                { stringToken (\s -> Real (read s :: Double)) }
  @ureal10                  { stringToken (\s -> String ("ureal" ++ s)) }
  @real10                   { stringToken (\s -> String ("real" ++ s)) }
  @complex10                { stringToken (\s -> String ("complex" ++ s)) }

  $dot                      { simpleToken Dot }
  @datumComment             { simpleToken CommentDatum }
  
  @identifier               { stringToken (\s -> Identifier s) }
}

<nestedComment> {
  @openingBlockComment   { pushStack nestedComment }
  @closingBlockComment   { popStack }

  (.|@lineEnding)         ;
}

{
-- Each action has type :: String -> Token

-- validateDelimiterToken :: (AlexInput -> Int -> a) -> AlexAction a
-- validateDelimiterToken f = token f `andPushStack` delimiter

simpleToken :: a -> AlexAction a
simpleToken t = token (\_ _ -> t)

stringToken :: (String -> a) -> AlexAction a
stringToken create = token (\(p,_,_,s) len -> create (take len s))

alexEOF :: Alex Token
alexEOF = return Eof

alexMonadListScan :: Alex [Token]
alexMonadListScan = do
  token <- alexMonadScan
  case token of Eof -> return []
                _ -> do
                  tokenList <- alexMonadListScan
                  return (token : tokenList)

data AlexUserState = AlexUserState {
  stateStack :: Stack.Stack Int
}

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState { stateStack=Stack.empty }

setStateStack :: Stack.Stack Int -> Alex ()
setStateStack stack = Alex $ \s -> Right (s{ alex_ust=(alex_ust s){ stateStack=stack } }, ())

getStateStack :: Alex (Stack.Stack Int)
getStateStack = Alex $ \s@AlexState{ alex_ust=ust } -> Right (s, stateStack ust)

andPushStack :: AlexAction a -> Int -> AlexAction a
(act `andPushStack` code) input len = do
  stack <- getStateStack
  currentStartCode <- alexGetStartCode
  setStateStack (Stack.push stack currentStartCode)
  alexSetStartCode code
  act input len

andPopStack :: AlexAction a -> AlexAction a
andPopStack act input len = do
  stack <- getStateStack
  let (code, alteredStack) = Stack.pop stack
  setStateStack alteredStack
  alexSetStartCode . maybe 0 id $ code
  act input len

popStack :: AlexAction Token
popStack = andPopStack skip

pushStack :: Int -> AlexAction Token
pushStack code = skip `andPushStack` code

scan :: String -> [Token]
scan str = case (runAlex str alexMonadListScan) of
            (Left msg) -> error msg
            (Right tokenList) -> tokenList
}
