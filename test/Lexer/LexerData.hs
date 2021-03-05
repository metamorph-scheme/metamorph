module Lexer.LexerData where

import Lexer.Token
import Common.Number

lexer1Result = [POpen, Define, Identifier "y", POpen, Lambda, POpen, Identifier "x", PClose, POpen,
                Identifier "*", (Number . Exact) (Integer 2),
                Identifier "x", PClose, PClose, PClose]

lexer2Result = [ POpen, Define, POpen, Identifier "lookup", Identifier "key-1", Identifier "key-2", Identifier "table", PClose
  , POpen, Identifier "let", POpen, POpen, Identifier "subtable", POpen, Identifier "assoc", Identifier "key-1"
  , POpen, Identifier "cdr", Identifier "table", PClose, PClose, PClose, PClose, POpen, If, Identifier "subtable"
  , POpen, Identifier "let", POpen, POpen, Identifier "record", POpen, Identifier "assoc", Identifier "key-2"
  , POpen, Identifier "cdr", Identifier "subtable", PClose, PClose, PClose, PClose, POpen, If, Identifier "record"
  , POpen, Identifier "cdr", Identifier "record", PClose, Bool False, PClose, PClose, Bool False
  , PClose, PClose, PClose ]
