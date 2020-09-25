module Lexer.TypeConversion where

import Lexer.Token
import Lexer.NumberParser
import Lexer.StringParser
import Lexer.Util

parseNumber :: String -> Token
parseNumber = makeErrorCallParser parseNumberE

parseBoolean :: String -> Token
parseBoolean "#f" = Bool False
parseBoolean "#t" = Bool True
parseBoolean "#false" = Bool False
parseBoolean "#true" = Bool True
parseBoolean s = error $ "cannot parse boolean from " ++ s

parseCharacter :: String -> Token
parseCharacter "#\\alarm" = Char '\a'
parseCharacter "#\\backspace" = Char '\b'
parseCharacter "#\\delete" = Char '\DEL'
parseCharacter "#\\escape" = Char '\ESC'
parseCharacter "#\\newline" = Char '\n'
parseCharacter "#\\null" = Char '\0'
parseCharacter "#\\return" = Char '\r'
parseCharacter "#\\space" = Char ' '
parseCharacter "#\\tab" = Char '\t'
parseCharacter s
  | (take 2 s) == "#\\" = parseCorrectCharacter s
  | otherwise = error $ "not a correct character " ++ s
  
parseCorrectCharacter :: String -> Token
parseCorrectCharacter s
  | (length s) == 3 = Char $ s !! 2
  | (s !! 2) == 'x' = Char $ getUnicodeEscapeCharacter (drop 3 s)
  | otherwise = error $ "cannot parse character from " ++ s

parseString :: String -> Token
parseString = makeErrorCallParser parseStringE

parseEscapedIdentifier :: String -> Token
parseEscapedIdentifier = makeErrorCallParser parseEscapedIdentifierE

makeErrorCallParser f s = case (f s) of
  (Left err) -> error . show $ err
  (Right token) -> token
