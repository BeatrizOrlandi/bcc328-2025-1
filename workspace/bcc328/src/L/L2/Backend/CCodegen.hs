{-# LANGUAGE LambdaCase #-}

module L.L2.Backend.CCodegen where

import L.L2.Frontend.Syntax
import Utils.Pretty (pretty)
import Utils.Value (Value (..))

-- top level code generation function
cL2Codegen :: L2 -> String
cL2Codegen e =
  unlines $
    [ "#include <stdio.h>"
    , "// code generated for expressions"
    , "int main () {"
    ]
      ++ (map (nest 4) (generateBody e))
      ++ [ nest 3 "return 0;"
         , "}"
         ]
 where
  nest n v = replicate n ' ' ++ v

generateBody :: L2 -> [String]
generateBody (L2 ss) = concatMap generateS2 ss

generateS2 :: S2 -> [String]
generateS2 (Def v e body) =
  ["{ int " ++ pretty v ++ " = " ++ pretty e ++ ";"]
    ++ (map (nest 4) (concatMap generateS2 body))
    ++ ["}"]
 where
  nest n v = replicate n ' ' ++ v
generateS2 (LRead s v) =
  [ "printf(\"" ++ s ++ "\\n\");"
  , "scanf(\"%d\", &" ++ pretty v ++ ");"
  ]
generateS2 (LPrint e) =
  case e of
    -- String literal
    LVal (VStr s) ->
      ["printf(\"" ++ s ++ "\\n\");"]
    _ ->
      ["printf(\"%d\\n\", " ++ pretty e ++ ");"]
generateS2 (LAssign v e) =
  ["int " ++ pretty v ++ " = " ++ pretty e ++ ";"]
