module Lexer.StringParser (parseStringE, parseEscapedIdentifierE) where

import Text.Parsec (parse, Parsec, try, ParseError)
import Text.Parsec.Char (oneOf, char, digit, hexDigit, letter, satisfy, string)
import Text.Parsec.Combinator (many1, choice, chainl1, optionMaybe)
import Control.Applicative ((<$>), (<*>), (<*), (*>), (<|>), many, (<$))
import Data.Maybe (catMaybes)

import Lexer.Token
import Lexer.Util

parseStringE :: String -> Either ParseError Token
parseStringE s = parse stringParser "String" s

parseEscapedIdentifierE :: String -> Either ParseError Token
parseEscapedIdentifierE s = parse escapedIdentifierParser "Escaped Identifier" s

escapedIdentifierParser = Identifier <$> (char '|' *> many symbolElementParser <* char '|')

symbolElementParser = try (satisfy (\c -> not . elem c $ "|\\"))
                      <|> try (inlineHexEscapeParser)
                      <|> try (mnemonicEscapeParser)
                      <|> try ('|' <$ string "\\|")

stringParser :: Parsec String st Token
stringParser = String <$> catMaybes <$> (char '"' *> many stringElementParser <* char '"')

stringElementParser :: Parsec String st (Maybe Char)
stringElementParser = try (Just <$> satisfy (\c -> not . elem c $ "\"\\"))
                      <|> try (Just '"' <$ string "\\\"")
                      <|> try (Just '\\' <$ string "\\\\")
                      <|> try (Nothing <$ (char '\\' *> many intralineWhitespaceParser *> lineEndingParser *> many intralineWhitespaceParser))
                      <|> try (Just <$> mnemonicEscapeParser)
                      <|> try (Just <$> inlineHexEscapeParser)

intralineWhitespaceParser :: Parsec String st Char
intralineWhitespaceParser = oneOf " \t"

mnemonicEscapeParser :: Parsec String st Char
mnemonicEscapeParser = try ('\a' <$ string "\\a")
                       <|> try ('\b' <$ string "\\b")
                       <|> try ('\t' <$ string "\\t")
                       <|> try ('\n' <$ string "\\n")
                       <|> try ('\r' <$ string "\\r")

inlineHexEscapeParser :: Parsec String st Char
inlineHexEscapeParser = getUnicodeEscapeCharacter <$> (string "\\x" *> many1 hexDigit <* char ';')

lineEndingParser :: Parsec String st String
lineEndingParser = try (string "\r\n") <|> string "\r" <|> string "\n"



optionalParser default_ parser = maybe default_ id <$> (optionMaybe parser)