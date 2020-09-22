module Data.Number (
  inexactNumber
  , exactNumber
  , defaultNumber
  , NumVal (..)
  , Number
  , InfReal (..)
) where

data Number = Exact NumVal | Inexact NumVal deriving (Eq, Show)
data NumVal = Integer Integer | Real InfReal deriving (Eq, Show)
data InfReal  = InfReal Double | PositiveInfinity | NegativeInfinity | PositiveNaN | NegativeNaN deriving (Eq, Show)

inexactNumber :: NumVal -> Number
inexactNumber n@(Integer _) = Inexact n
inexactNumber n@(Real _) = Inexact n

exactNumber :: NumVal -> Number
exactNumber n@(Integer _) = Exact n
exactNumber (Real _) = error "cannot make exact real"

defaultNumber :: NumVal -> Number
defaultNumber n@(Integer _) = Exact n
defaultNumber n@(Real _) = Inexact n