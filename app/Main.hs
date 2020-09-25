module Main where

import Lib
import Parser.Parser
import Lexer.Lexer
import System.IO

main :: IO ()
main = do
    text <- getContents 
    let tokens = scan text 
    putStrLn . show . parseScheme $ tokens 
