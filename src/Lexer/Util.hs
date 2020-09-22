module Lexer.Util where

import Data.Char
import Numeric

-- unicode escape for character parsing and unicode escape in identifiers (must be general)
-- using chr from Data.Char and Numeric.readHex
getUnicodeEscapeCharacter :: String -> Char
getUnicodeEscapeCharacter s = chr . fst . head . readHex $ s