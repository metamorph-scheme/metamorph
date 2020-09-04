module Lexer.Token where

import Data.Complex

-- The token type:
data Token  = Lambda | If | Set | POpen | PClose | Identifier String | Quote | ShortQuote | Integral Int | Rational Int Int
            | Real Double | String String | Complex (Complex Double)  | Bool Bool | Char Char
            | Dot | QuasiQuote | ShortQuasiQuote | Unquote | ShortUnquote | UnquoteSplice | CommentDatum
            | ShortUnquoteSplice | Label Integer | LabelRef Integer | Define | Eof deriving (Eq, Show)
