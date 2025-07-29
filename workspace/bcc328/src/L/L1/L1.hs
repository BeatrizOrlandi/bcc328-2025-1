import L.L1.Backend.CCodegen
import L.L1.Backend.V1Codegen
import L.L1.Interpreter.Interp
import L.L1.Frontend.Lexer
import L.L1.Frontend.RecursiveParser
import L.L1.Frontend.Syntax
import Utils.Pretty
import Utils.Repl
import Utils.Value
import V.V0.Instr
import Text.Printf (printf)
import System.Environment
import System.FilePath
import System.Process
import Happy.L1Parser (parseL1)

main :: IO ()
main = do
  args <- getArgs
  let opts = parseOptions args
  runWithOptions opts

-- running the compiler / interpreter

runWithOptions :: [Option] -> IO ()
runWithOptions opts = case opts of
  [Lexer file] ->
    alexBasedLexer file
  [Recursive file] ->
    recursiveParser file
  [LALR file] ->
    lalrParser file
  [VM file] ->
    v1Compiler file
  _ -> helpMessage


-- Implement the function to do lexical analysis for L1 programs

alexBasedLexer :: FilePath -> IO ()
alexBasedLexer file = do
  source <- readFile file
  let toks = lexer source
  mapM_ printTok toks
  where
    printTok (Token (l,c) lx) =
      putStrLn $ printf "%-25s Linha:%-3d Coluna:%d" (showLex lx) l c

    showLex :: Lexeme -> String
    showLex le = case le of
      TNumber n   -> "Número "            ++ show n
      TIdent v    -> "Identificador "     ++ v
      TString s   -> "String \""          ++ s ++ "\""
      TAssign     -> "Atribuição :="
      TSemi       -> "Ponto e vírgula ;"
      TComma      -> "Vírgula ,"
      TLParen     -> "Parêntese ("
      TRParen     -> "Parêntese )"
      TPlus       -> "Operador +"
      TMinus      -> "Operador -"
      TTimes      -> "Operador *"
      TDiv        -> "Operador /"
      TRead       -> "Palavra reservada read"
      TPrint      -> "Palavra reservada print"
      TEOF        -> "EOF"



-- Implement the function to do syntax analysis using a recursive parser

recursiveParser :: FilePath -> IO ()
recursiveParser file = do
  src <- readFile file
  case l1Parser src of
    Left err -> do
      putStrLn "Erro de parse:\n"
      putStrLn err           -- mostra mensagem e continua
    Right ast -> do
      putStrLn "Árvore de sintaxe produzida:\n"
      print ast              -- Show derivado da AST

-- Implement the LALR parser

lalrParser :: FilePath -> IO ()
lalrParser file = do
  src <- readFile file
  case parseL1 src of
    Left err   -> putStrLn $ "Erro LALR:\n" ++ err
    Right ast  -> print ast

-- Implement the V1 code generator

v1Compiler :: FilePath -> IO ()
v1Compiler file = error "Not implemented!"

-- help message

helpMessage :: IO ()
helpMessage
  = putStrLn $ unlines [ "L1 language"
                       , "Usage: l1 [--lexer-only FILE | --recursive FILE  | --lalr FILE | -- v1 FILE | --help]"
                       , "--lexer-only: does the lexical analysis of the input programming using a Alex based lexer."
                       , "--recursive: does the syntax analysis using a recursive descendent Megaparsec parser."
                       , "--lalr: does the syntax analysis using a LALR parser."
                       , "--v1: Translate L1 code into V1 instructions."
                       , "--help: prints this help message."
                       ]

-- parse command line arguments

data Option
  = Help
  | Lexer FilePath
  | Recursive FilePath
  | LALR FilePath
  | VM FilePath
  deriving (Eq, Show)

parseOptions :: [String] -> [Option]
parseOptions args =
  case args of
    ("--lexer-only" : arg : _) -> [Lexer arg]
    ("--recursive" : arg : _) -> [Recursive arg]
    ("--lalr" : arg : _) -> [LALR arg]
    ("--v1" : arg : _) -> [VM arg]
    _ -> [Help]
