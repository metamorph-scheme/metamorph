module Lib
( someFunc
, returnThree
) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

returnThree :: a -> Int
returnThree _ = 3