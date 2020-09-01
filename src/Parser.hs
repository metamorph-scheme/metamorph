module Parser where

data Token = Lambda | If | Set | POpen | PClose | Identifier | Quote | ShortQuote 
            | Num Number | String String | Complex Complex | Bool Bool | Char Char | Symbol String
            | Point | QuasiQuote | ShortQuasiQuote | Unquote | ShortUnquote | UnquoteSplice 
            | ShortUnquoteSplice | Label Integer | LabelRef Integer