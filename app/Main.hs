module Main where

import System.IO
import System.Process
import System.Console.ArgParser
import Shelly

import Backends

import Lexer.Lexer
import Parser.Parser
import SemanticAnalysis.SemanticAnalysis
import CodeGeneration.CodeGeneration

data Metamorph = Metamorph String String String String Bool

parameterParser :: ParserSpec Metamorph
parameterParser = Metamorph
  `parsedBy` reqPos "scheme-source"
  `andBy` optFlag "./clib" "clib"
  `andBy` optFlag "./out" "outfolder"
  `andBy` optFlag "gcc" "backend"
  `andBy` boolFlag "run-backend"
  
main :: IO ()
main = withParseResult parameterParser runMetamorph
  
runMetamorph :: Metamorph -> IO ()
runMetamorph (Metamorph schemePath clibPath outFolder backend runBackend) = do
  shelly $ do
    rm_rf outFolder
    mkdir_p outFolder
    cp_r (clibPath ++ "/metamorph-clib/src") outFolder
  schemeSource <- readFile $ schemePath
  writeFile (outFolder ++ "/src/main.c"). metamorph $ schemeSource
  if runBackend then
    case backend of
      "gcc" -> callCommand (gccCompileUnix outFolder)
  else return ()

-- pure metamorph source-to-source translation function
metamorph :: String -> String
metamorph = generate . semanticAnalysis . parseScheme . scan

