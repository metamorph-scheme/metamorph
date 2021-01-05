module Main where

import Lib
import Parser.Parser
import Parser.MetaNode
import Lexer.Lexer
import System.IO
import SemanticAnalysis.SemanticAnalysis

main :: IO ()
main = do
    putStrLn "Enter Path:"
    path <- getLine 
    text <- readFile path
    let tokens = scan text 
    putStrLn . show . semanticAnalysis . parseScheme $ tokens
