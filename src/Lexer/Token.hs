module Lexer.Token where

import Common.Number

-- The token type:
data Token  = Lambda | If | Set | POpen | PClose | Identifier String | Quote | ShortQuote
            | Number Number | String String | Bool Bool | Char Char
            | Dot | QuasiQuote | ShortQuasiQuote | Unquote | ShortUnquote | UnquoteSplice | CommentDatum
            | ShortUnquoteSplice | Label Integer | LabelRef Integer | Define | Eof deriving (Eq, Show, Read)