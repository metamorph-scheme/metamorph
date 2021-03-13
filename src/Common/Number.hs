module Common.Number (
  defaultNumber
  , NumVal (..)
  , Number (..)
  , InfReal (..)
) where

data Number = Exact NumVal | Inexact NumVal deriving (Eq, Show, Read)
data NumVal = Integer Integer | Real InfReal | Rational Integer Integer deriving (Eq, Show, Read)
data InfReal  = InfReal Double | PositiveInfinity | NegativeInfinity | PositiveNaN | NegativeNaN deriving (Eq, Show, Read)

defaultNumber :: NumVal -> Number
defaultNumber n@(Integer _) = Exact n
defaultNumber n@(Rational _ _) = Exact n
defaultNumber n@(Real _) = Inexact n