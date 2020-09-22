module Lexer.TypeConversionSpec where

import Lexer.TypeConversion
import Lexer.Token
import Data.Complex
import Data.Number

import Text.Parsec (parse, Parsec, try, ParseError)

import Control.Exception (evaluate)
import Test.Hspec

spec :: Spec
spec = do

  describe "parseBoolean" $ do
    it "can parse short true" $ do
      parseBoolean "#t" `shouldBe` (Bool True)
    it "can parse short false" $ do
      parseBoolean "#f" `shouldBe` (Bool False)
    it "can parse long true" $ do
      parseBoolean "#true" `shouldBe` (Bool True)
    it "can parse long false" $ do
      parseBoolean "#false" `shouldBe` (Bool False)
    it "rejects non-boolean" $ do
      evaluate (parseBoolean "#blah") `shouldThrow` anyErrorCall

  describe "parseCharacter" $ do
    it "can parse single character" $ do
      parseCharacter "#\\e" `shouldBe` (Char 'e')

    it "can parse single unicode character" $ do
      parseCharacter "#\\\3619" `shouldBe` (Char '\3619')

    it "can parse single x" $ do
      parseCharacter "#\\x" `shouldBe` (Char 'x')

    it "can parse alarm" $ do
      parseCharacter "#\\alarm" `shouldBe` (Char '\a')

    it "can parse backspace" $ do
      parseCharacter "#\\backspace" `shouldBe` (Char '\b')

    it "can parse delete" $ do
      parseCharacter "#\\delete" `shouldBe` (Char '\DEL')

    it "can parse escape" $ do
      parseCharacter "#\\escape" `shouldBe` (Char '\ESC')

    it "can parse newline" $ do
      parseCharacter "#\\newline" `shouldBe` (Char '\n')

    it "can parse null" $ do
      parseCharacter "#\\null" `shouldBe` (Char '\0')

    it "can parse return" $ do
      parseCharacter "#\\return" `shouldBe` (Char '\r')

    it "can parse space" $ do
      parseCharacter "#\\space" `shouldBe` (Char ' ')

    it "can parse tab" $ do
      parseCharacter "#\\tab" `shouldBe` (Char '\t')

    it "can parse unicode escape" $ do
      parseCharacter "#\\x3a" `shouldBe` (Char ':')

    it "can parse chinese unicode escape" $ do
      parseCharacter "#\\x3AED" `shouldBe` (Char '\15085')

    it "can parse unicode escape with leading zero" $ do
      parseCharacter "#\\x003a" `shouldBe` (Char ':')

  describe "parseNumber" $ do
    it "can parse integer" $ do
      parseNumber "32\0" `shouldBe` (Number . exactNumber $ Integer 32)

    it "can parse negative integer" $ do
      parseNumber "-32\0" `shouldBe` (Number . exactNumber $ Integer (-32))

    it "can parse inexact integer" $ do
      parseNumber "#i3\0" `shouldBe` (Number . inexactNumber $ Integer 3)

    it "can parse exact integer" $ do
      parseNumber "#e3\0" `shouldBe` (Number . exactNumber $ Integer 3)

    it "can parse inexact decimal real" $ do
      parseNumber "#i3.141\0" `shouldBe` (Number . inexactNumber . Real $ InfReal 3.141)

    it "can parse decimal" $ do
      parseNumber "141e2\0" `shouldBe` (Number . inexactNumber . Real $ InfReal 14100)

    it "can parse decimal point only" $ do
      parseNumber ".131\0" `shouldBe` (Number . inexactNumber . Real $ InfReal 0.131)

    it "can parse decimal point only suffix" $ do
      parseNumber ".131e1\0" `shouldBe` (Number . inexactNumber . Real $ InfReal 1.31)

    it "can parse decimal point" $ do
      parseNumber "3.131\0" `shouldBe` (Number . inexactNumber . Real $ InfReal 3.131)

    it "can parse inexact decimal point" $ do
      parseNumber "#i3.131\0" `shouldBe` (Number . inexactNumber . Real $ InfReal 3.131)

    it "can parse inexact decimal point suffix" $ do
      parseNumber "#i3.131e2\0" `shouldBe` (Number . inexactNumber . Real $ InfReal 313.1)

    it "can parse rational" $ do
      parseNumber "1/2\0" `shouldBe` (Number . inexactNumber . Real $ InfReal 0.5)

    it "can parse inexact rational" $ do
      parseNumber "#i1/2\0" `shouldBe` (Number . inexactNumber . Real $ InfReal 0.5)

  describe "parseString" $ do

    it "can parse string" $ do
      parseString "\"hello world\"" `shouldBe` String "hello world"

    it "can parse string with escaped quote" $ do
      parseString "\"hello\\\" world\"" `shouldBe` String "hello\" world"

    it "can parse string with escaped backslash" $ do
      parseString "\"hello\\\\ world\"" `shouldBe` String "hello\\ world"

    it "can parse mnemonic escape" $ do
      parseString "\"\\a\\b\\t\\n\\r\"" `shouldBe` String "\a\b\t\n\r"

    it "can parse unicode escape" $ do
      parseString "\"a\\x003a;\"" `shouldBe` String "a:"

    it "can parse multiline string" $ do
      parseString "\"ab\\  \n  c\"" `shouldBe` String "abc"

  describe "parseEscapedIdentifier" $ do

    it "can parse escaped identifier" $ do
      parseEscapedIdentifier "|hello abc \t\\| \\x003a; \\a \\b |" `shouldBe` Identifier "hello abc \t| : \a \b "
