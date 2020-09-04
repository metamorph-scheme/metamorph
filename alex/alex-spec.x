{
module Lexer.Lexer (scan) where

import Lexer.Token
}

%wrapper "monad"

-- character sets

$digit = 0-9
$letter = [a-zA-Z]
$specialInitial = [!\$\%&\*\/:\<\=\>\?\^\~]
$mnemonicEscape = [\\a\\b\\t\\n\\r]
$dot = \.
$hexDigit = [0-9abcdef]
$verticalLine = \|
$intralineWhitespace = [\ \t]
$openingBracket = \(
$closingBracket = \)
$explicitSign = [\+\-]

-- general

@lineEnding = \n | \r\n | \r
@whitespace = $intralineWhitespace | @lineEnding
@directive = \#!fold-case | \#!no-fold-case
@characterName = (alarm|backspace|delete|escape|newline|null|return|space|tab)
-- @nestedComment = \#\| (.|@lineEnding)* \|\# -- we dont care about properly balanced nested comments
@comment = \;.* -- | @nestedComment | \#\; @intertokenSpace @datum
@atmosphere = @whitespace | @comment | @directive
@intertokenSpace = @atmosphere*

-- identifier

@initial = $letter | $specialInitial
@specialSubsequent = $explicitSign | $dot | @
@subsequent = @initial | $digit | @specialSubsequent
@hexScalarValue = $hexDigit+
@inlineHexExcape = \\x(@hexScalarValue)\;
@symbolElement = [^$verticalLine\\] | @inlineHexExcape | $mnemonicEscape | \\\|
@signSubsequent = @initial | $explicitSign | @
@dotSubsequent = @signSubsequent | $dot
@peculiarIdentifier = $explicitSign
                      | $explicitSign @signSubsequent @subsequent*
                      | $explicitSign $dot @dotSubsequent @subsequent*
                      | $dot @dotSubsequent @subsequent*
@identifier = @initial @subsequent*
              | $verticalLine @symbolElement+ $verticalLine
              | @peculiarIdentifier

-- primitive expressions

@lambda = lambda
@set = set!
@if = if

tokens :-


  @whitespace+				;      -- ignore whitespace
	@comment 						;

  $openingBracket        { simpleToken POpen }  
  $closingBracket        { simpleToken PClose }
  @lambda                { simpleToken Lambda }
  @set                   { simpleToken Set }
  @if                    { simpleToken If }
  $dot                   { simpleToken Dot }
  @identifier            { token (\(p,_,_,str) _ -> Identifier str) }
	.+									   { token (\(p,_,_,str) _ -> String str) }


{
-- Each action has type :: String -> Token

simpleToken :: a -> AlexAction a
simpleToken t = token (\_ _ -> t)

alexEOF :: Alex Token
alexEOF = return Eof

alexMonadListScan :: Alex [Token]
alexMonadListScan = do
  token <- alexMonadScan
  case token of Eof -> return []
                _ -> do
                  tokenList <- alexMonadListScan
                  return (token : tokenList)


scan :: String -> [Token]
scan str = case (runAlex str alexMonadListScan) of
            (Left msg) -> error msg
            (Right tokenList) -> tokenList
}