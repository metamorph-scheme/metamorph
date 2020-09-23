{
module Lexer.Lexer (scan) where

import Lexer.Token
import qualified Data.Stack as Stack
import Lexer.TypeConversion
}

%wrapper "monadUserState"

-- character sets

$digit = [0-9]
$letter = [a-zA-Z]
$specialInitial = [!\$\%&\*\/:\<\=\>\?\^\~]
$dot = \.
$hexDigit = [0-9a-f]
$verticalLine = \|
$intralineWhitespace = [\ \t]
$openingBracket = \(
$closingBracket = \)
$explicitSign = [\+\-]

-- general

@lineEnding = (\n | \r\n | \r)
@mnemonicEscape = (\\a|\\b|\\t|\\n|\\r)
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
@symbolElement = ([^$verticalLine\\] | @inlineHexExcape | @mnemonicEscape | \\\|)
@signSubsequent = (@initial | $explicitSign | @)
@dotSubsequent = (@signSubsequent | $dot)
@peculiarIdentifier = ($explicitSign
                      | $explicitSign @signSubsequent @subsequent*
                      | $explicitSign $dot @dotSubsequent @subsequent*
                      | $dot @dotSubsequent @subsequent*)
@identifier = (@initial @subsequent*
              | @peculiarIdentifier)
@verticalLineIdentifier = $verticalLine @symbolElement+ $verticalLine

-- primitive expressions

@lambda = lambda
@set = set!
@if = if

-- quotation

@quote = quote
@shortQuote = '
@quasiquote = quasiquote
@shortQuasiquote = `
@unquote = unquote
@shortUnquote = \,
@unquoteSplicing = unquote\-splicing
@shortUnquoteSplicing = \,@

@define = define

-- simple literals

@boolean = (\#t | \#f | \#true | \#false)
@character = ( \#\\ .
             | \#\\ @characterName
             | \#\\x @hexScalarValue)
@stringElement = ( [^\"\\]
                 | @mnemonicEscape
                 | \\\"
                 | \\\\
                 | \\ $intralineWhitespace* @lineEnding $intralineWhitespace*
                 | @inlineHexExcape)
@string = \" @stringElement* \"

@byte = ([0-9]{1,3})
@bytevector = \#u8\( @byte* \)

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

@uinteger2 = $digit2+
@uinteger8 = $digit8+
@uinteger10 = $digit10+
@uinteger16 = $digit16+

@decimal10 = (@uinteger10 @suffix | $dot $digit10+ @suffix | $digit10+ $dot $digit10* @suffix)

@ureal2 = (@uinteger2 | @uinteger2 \/ @uinteger2)
@ureal8 = (@uinteger8 | @uinteger8 \/ @uinteger8)
@ureal10 = (@uinteger10 | @uinteger10 \/ @uinteger10 | @decimal10)
@ureal16 = (@uinteger16 | @uinteger16 \/ @uinteger16)

@real2 = (@sign @ureal2 | @infnan)
@real8 = (@sign @ureal8 | @infnan)
@real10 = (@sign @ureal10 | @infnan)
@real16 = (@sign @ureal16 | @infnan)

@complex2 = ( @real2
             | @real2 @ @real2 
             | @real2 \+ @real2 i
             | @real2 \- @real2 i
             | @real2 \+ i
             | @real2 \- i
             | @real2 @infnan i
             | \+ @ureal2 i
             | \- @ureal2 i
             | @infnan i
             | \+ i
             | \- i)
@complex8 = ( @real8
             | @real8 @ @real8 
             | @real8 \+ @real8 i
             | @real8 \- @real8 i
             | @real8 \+ i
             | @real8 \- i
             | @real8 @infnan i
             | \+ @ureal8 i
             | \- @ureal8 i
             | @infnan i
             | \+ i
             | \- i)
@complex10 = ( @real10
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
@complex16 = ( @real16
             | @real16 @ @real16 
             | @real16 \+ @real16 i
             | @real16 \- @real16 i
             | @real16 \+ i
             | @real16 \- i
             | @real16 @infnan i
             | \+ @ureal16 i
             | \- @ureal16 i
             | @infnan i
             | \+ i
             | \- i)

@num2 = @prefix2 @complex2
@num8 = @prefix8 @complex8
@num10 = @prefix10 @complex10
@num16 = @prefix16 @complex16

@number = ( @num2 | @num8 | @num10 | @num16 )

tokens :-

<0> {
  @whitespace+				;      -- ignore whitespace
	@comment 						;
  @openingBlockComment      { pushStack nestedComment }

  $openingBracket           { simpleToken POpen }  
  $closingBracket           { simpleToken PClose }
  @lambda / @delimiter      { simpleToken Lambda }
  @set / @delimiter         { simpleToken Set }
  @if / @delimiter          { simpleToken If }
  @define / @delimiter      { simpleToken Define }

  @quote / @delimiter                   { simpleToken Quote }
  @shortQuote                           { simpleToken ShortQuote }
  @quasiquote / @delimiter              { simpleToken QuasiQuote }
  @shortQuasiquote                      { simpleToken ShortQuasiQuote }
  @unquote / @delimiter                 { simpleToken Unquote }
  @shortUnquote                         { simpleToken ShortUnquote }
  @unquoteSplicing / @delimiter         { simpleToken UnquoteSplice }
  @shortUnquoteSplicing                 { simpleToken ShortUnquoteSplice }

  @number / @delimiter      { stringToken (\s -> parseNumber (s ++ "\0")) }

  @boolean / @delimiter     { stringToken (\s -> parseBoolean s ) }
  @string                   { stringToken (\s -> parseString s) }
  @character / @delimiter   { stringToken (\s -> parseCharacter s)}

  $dot / @delimiter         { simpleToken Dot }
  @datumComment             { simpleToken CommentDatum }
  
  @identifier / @delimiter  { stringToken (\s -> Identifier s) }
  @verticalLineIdentifier   { stringToken (\s -> parseEscapedIdentifier s)}
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
