{-# LANGUAGE LambdaCase #-}

import L.L2.Frontend.Lexer    (Token(..), Lexeme(..), lexer)
import Happy.L2Parser         (parseL2)
import L.L2.Frontend.TypeCheck (typeCheck)
import L.L2.Interpreter.Interp (runL2)
import L.L2.Backend.V1Codegen (v1Codegen)
import L.L2.Backend.CCodegen  (cL2Codegen)
import System.FilePath (replaceExtension)
import Utils.Pretty

import System.Environment (getArgs)
import System.Process      (callCommand)
import Text.Printf         (printf)

import qualified V.V1.V1Parser as V1P   
import qualified V.V1.Interp   as V1I   
import qualified V.V1.V1Lexer as V1L

lexerOnly :: FilePath -> IO ()
lexerOnly file = do
  src <- readFile file
  mapM_ printTok (lexer src)
  where
    printTok (Token (l,c) lx) =
      putStrLn $ printf "%-20s Linha:%-3d Coluna:%d" (show lx) l c

parserOnly :: FilePath -> IO ()
parserOnly file = do
  src <- readFile file
  case parseL2 src of
    Left err -> putStrLn err
    Right ast -> print ast

interpret :: FilePath -> IO ()
interpret file = do
  src <- readFile file
  case parseL2 src >>= typeCheck of
    Left err  -> putStrLn $ "Erro: " ++ err
    Right ast -> runL2 ast

-- 3. Compilador V1  --------------------------------------------------------
v1Compiler :: FilePath -> IO ()
v1Compiler file = do
  src <- readFile file
  case parseL2 src >>= typeCheck of
    Left err  -> putStrLn err
    Right ast -> do
      let instrs = v1Codegen ast           
          v1Src  = unlines instrs         
          out    = replaceExtension file ".v1"
      writeFile out v1Src
      putStrLn $ "V1 code written to " ++ out

-- 4. Compilador C  ---------------------------------------------------------
cCompiler :: FilePath -> IO ()
cCompiler file = do
  src <- readFile file
  case parseL2 src >>= typeCheck of
    Left err  -> putStrLn err
    Right ast -> do
      let cFile  = replaceExtension file ".c"
          exe    = replaceExtension file ""
      writeFile cFile (cL2Codegen ast)
      putStrLn $ "Código C salvo em " ++ cFile
      callCommand $ unwords ["gcc -std=c99 -O2", cFile, "-o", exe]
      putStrLn $ "Executável gerado: " ++ exe

main :: IO ()
main = getArgs >>= \case
  ["--lexer-only",   f] -> lexerOnly   f
  ["--parser-only",  f] -> parserOnly  f
  ["--interpret",    f] -> interpret   f
  ["--compile-v1",   f] -> v1Compiler  f
  ["--compile-c",    f] -> cCompiler   f
  _ -> putStrLn $ unlines
        [ "Uso:"
        , "  l2 --lexer-only   <arquivo>"
        , "  l2 --parser-only  <arquivo>"
        , "  l2 --interpret    <arquivo>"
        , "  l2 --compile-v1   <arquivo>"
        , "  l2 --compile-c    <arquivo>"
        ]