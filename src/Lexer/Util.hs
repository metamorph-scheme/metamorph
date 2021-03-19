module Lexer.Util where

import Data.Char
import Common.Number
import Numeric

-- unicode escape for character parsing and unicode escape in identifiers (must be general)
-- using chr from Data.Char and Numeric.readHex
getUnicodeEscapeCharacter :: String -> Char
getUnicodeEscapeCharacter s = chr . fst . head . readHex $ s

inexactNumber :: NumVal -> Number
inexactNumber n@(Integer _) = Inexact n
inexactNumber n@(Real _) = Inexact n
inexactNumber n@(Rational _ _) = Inexact n

exactNumber :: NumVal -> Number
exactNumber n@(Integer _) = Exact n
exactNumber n@(Rational _ _) = Exact n
exactNumber (Real _) = error "cannot make exact real"