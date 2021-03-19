module Lexer.LexerSpec where

import Lexer.Lexer
import Lexer.Token
import Common.Number
import System.IO

import Control.Exception (evaluate)
import Lexer.LexerData
import Test.Hspec


spec :: Spec
spec = describe "Lexer.scan" $ do
  it "can discard whitespace" $ do
    scan "\n\t\r    " `shouldBe` []
  
  it "can discard single line comment" $ do
    scan "; my comment \nidentifier " `shouldBe` [Identifier "identifier"]
  
  describe "block comment" $ do
    it "can discard block comment" $ do
      scan "#| my \n block \n comment |# identifier " `shouldBe` [Identifier "identifier"]

    it "can discard single line block comment" $ do
      scan "#| my block comment |# identifier " `shouldBe` [Identifier "identifier"]

    it "can discard block comment lazily" $ do
      scan "#| my comment |# identa #| comment 2 |#" `shouldBe` [Identifier "identa"]
  
  it "can discard whitespace and comment" $ do
    scan "; my comment \n   \tidentifier " `shouldBe` [Identifier "identifier"]
  
  it "can classify opening bracket" $ do
    scan "(" `shouldBe` [POpen]

  it "can classify closing bracket" $ do
    scan ")" `shouldBe` [PClose]

  describe "identifiers" $ do
    it "can classify identifier" $ do
      scan "identifier " `shouldBe` [Identifier "identifier"]
    
    it "can classify two identifiers" $ do
      scan "identa ident2 " `shouldBe` [Identifier "identa", Identifier "ident2"]

    it "can classify underscore identifiers" $ do
      scan "_ " `shouldBe` [Identifier "_"]

    it "can classify identifier followed by bracket" $ do
      scan "identifier(a)" `shouldBe` [Identifier "identifier", POpen, Identifier "a", PClose]

    it "rejects prohibited initial character" $ do
      evaluate (scan "5dentifier") `shouldThrow` anyErrorCall

    it "can classify escaped identifier" $ do
      scan "|hello abc \t\\| \\x003a; \\a \\b |" `shouldBe` [ Identifier "hello abc \t| : \a \b " ]

  it "can classify lambda" $ do
    scan "lambda " `shouldBe` [Lambda]

  it "can classify set" $ do
    scan "set! " `shouldBe` [Set]

  it "can classify if" $ do
    scan "if " `shouldBe` [If]

  it "can classify dot" $ do
    scan ". " `shouldBe` [Dot]

  it "can classify define" $ do
    scan "define " `shouldBe` [Define]
  
  it "can classify define with bracket" $ do
    scan "define(" `shouldBe` [Define, POpen ]

  describe "datum comment" $ do
    it "can classify datum comment" $ do
      scan "#;" `shouldBe` [CommentDatum]
    
    it "can classify datum comment with block comment" $ do
      scan "#;#| abc |#(a)" `shouldBe`
        [ CommentDatum
        , POpen
        , Identifier "a"
        , PClose
        ]

    it "can classify datum comment with whitespace" $ do
      scan "#;  \t\n   (a)" `shouldBe`
        [ CommentDatum
        , POpen
        , Identifier "a"
        , PClose
        ]

    it "can classify datum comment with whitespace and block comment" $ do
      scan "#;#| abc |#     (a)" `shouldBe`
        [ CommentDatum
        , POpen
        , Identifier "a"
        , PClose
        ]

  describe "uinteger" $ do
    it "can classify uinteger" $ do
      scan "324 " `shouldBe` [ Number . Exact $ Integer 324 ]

  describe "decimal" $ do
    it "can classify decimal" $ do
      scan "3.56 " `shouldBe` [ Number . Inexact . Real $ InfReal 3.56 ]
  
  describe "ureal" $ do
    it "can classify ureal" $ do
      scan "4/2 " `shouldBe` [ Number . Exact $ Rational 4 2 ]
  
  describe "real" $ do
    it "can classify real" $ do
      scan "+inf.0 " `shouldBe` [ Number . Inexact . Real $ PositiveInfinity ]

  describe "string" $ do
    it "can classify string" $ do
      scan "\"hello world\"" `shouldBe` [ String "hello world" ]

    it "can classify string with escaped quote" $ do
      scan "\"hello\\\" world\"" `shouldBe` [ String "hello\" world" ]

    it "can classify mnemonic escape" $ do
      scan "\"\\a\"" `shouldBe` [ String "\a" ]

  describe "quotation" $ do
    it "can classify quote" $ do
      scan "quote " `shouldBe` [ Quote ]
     
    it "can classify short quote" $ do
      scan "'(" `shouldBe` [ ShortQuote, POpen]
     
    it "can classify quasiquote" $ do
      scan "quasiquote " `shouldBe` [ QuasiQuote ]
     
    it "can classify short quasiquote" $ do
      scan "` ( " `shouldBe` [ ShortQuasiQuote, POpen ]
     
    it "can classify unquote" $ do
      scan "unquote " `shouldBe` [ Unquote ]
     
    it "can classify short unquote" $ do
      scan ",( " `shouldBe` [ ShortUnquote, POpen ]
     
    it "can classify unquote-splicing" $ do
      scan "unquote-splicing " `shouldBe` [ UnquoteSplice ]
     
    it "can classify short unquote-splicing" $ do
      scan ",@( " `shouldBe` [ ShortUnquoteSplice, POpen ]

  describe "real file" $ do
    it "can classify real scm file" $ do
      withFile "testdata/lexer1.scm" ReadMode (\handle -> do
        content <- hGetContents handle
        scan content `shouldBe` lexer1Result )

    it "can classify real scm file" $ do
      withFile "testdata/sicp-representing-tables.scm" ReadMode (\handle -> do
        content <- hGetContents handle
        scan content `shouldBe` lexer2Result )