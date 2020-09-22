module Lexer.NumberParser (parseNumberE) where

import Data.Number as N
import Text.Parsec (parse, Parsec, try, ParseError)
import Text.Parsec.Char (oneOf, char, digit, hexDigit, letter, satisfy, string)
import Text.Parsec.Combinator (many1, choice, chainl1, optionMaybe)
import Control.Applicative ((<$>), (<*>), (<*), (*>), (<|>), many, (<$))

import Lexer.Token

parseNumberE :: String -> Either ParseError Token
parseNumberE s = parse numberParser "Number" s

numberParser :: Parsec String st Token
numberParser = Number <$> (exactnessParser <*> (try (realParser 10) <|> integerParser 10)) <* char '\0'

signParser :: (Num a) => Parsec String st (a -> a)
signParser = optionalParser positive $ positive <$ char '+' <|> negative <$ char '-'
  where positive = id
        negative = negate

exactnessParser :: Parsec String st (N.NumVal -> N.Number)
exactnessParser = optionalParser N.defaultNumber $ (exact <$ try (string "#e")) <|> (inexact <$ string "#i")
  where exact = N.exactNumber
        inexact = N.inexactNumber

integerParser :: Int -> Parsec String st N.NumVal
integerParser base = N.Integer <$> (signParser <*> uintegerParser base)

uintegerParser :: (Num a, Read a) => Int -> Parsec String st a
uintegerParser 10 = read <$> many1 digit
--uintegerParser 16 = (fst . head . readHex) <$> many1 hexDigit

realParser :: Int -> Parsec String st N.NumVal
realParser 10 = N.Real <$> (try (N.InfReal <$> (signParser <*> (try rationalParser <|> decimalParser ))) <|> infnanParser)
  where rationalParser = (/) <$> uintegerParser 10 <* char '/' <*> uintegerParser 10

infnanParser :: Parsec String st N.InfReal
infnanParser = try (N.PositiveInfinity <$ string "+inf.0")
               <|> N.NegativeInfinity <$ string "-inf.0"
               <|> N.PositiveNaN <$ string "+nan.0"
               <|> N.NegativeNaN <$ string "-nan.0"

--decimalParser :: Parsec String st Double
decimalParser :: Parsec String st Double
decimalParser = try decimalPointParser <|> decimalSuffixParser <|> decimalOnlyPointParser
    
decimalSuffixParser :: Parsec String st Double
decimalSuffixParser = exponentFunc <$> uintegerParser 10 <*> suffixParser

decimalOnlyPointParser :: Parsec String st Double
decimalOnlyPointParser = exponentFunc <$> (char '.' *> (toDecimalPlaces <$> map (read . (:[])) <$> many1 digit)) <*> optionalParser 0 suffixParser

--decimalPointParser = exponentFunc <$> ((+) <$> uintegerParser 10 <*> (char '.' *> (toDecimalPlaces <$> map (read . (:[])) <$> many digit))) <*> optionalParser 0 suffixParser
decimalPointParser :: Parsec String st Double
decimalPointParser = exponentFunc <$> read <$> ((++) <$> many1 digit <*> ((:) <$> char '.' <*> many digit)) <*> optionalParser 0 suffixParser

suffixParser :: (Num a, Read a) => Parsec String st a
suffixParser = char 'e' *> (signParser <*> uintegerParser 10)

toDecimalPlaces xs = sum . map (\(a,b) -> b a) $ zip xs [(/(10^i)) | i <- [1..]]
countDigits n = (+1) . floor $ log n / log 10
exponentFunc = (\n e -> n * 10 ^^ e)
optionalParser default_ parser = maybe default_ id <$> (optionMaybe parser)