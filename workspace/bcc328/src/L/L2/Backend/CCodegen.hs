{-# LANGUAGE LambdaCase #-}

module L.L2.Backend.CCodegen (cL2Codegen) where

import L.L2.Frontend.Syntax
import Utils.Pretty (pretty)
import Utils.Value  (Value(..))

cL2Codegen :: L2 -> String
cL2Codegen e =
  unlines $
    [ "#include <stdio.h>"
    , "int main () {"
    ]
    ++ map (indent 2) (generateBody e)
    ++ [ indent 2 "return 0;"
       , "}"
       ]
  where
    indent n = (replicate (n*2) ' ' ++)

generateBody :: L2 -> [String]
generateBody (L2 ss) = concatMap genStmt ss

genStmt :: S2 -> [String]
genStmt = \case
  Def v e body ->
       ["{ int " ++ pretty v ++ " = " ++ goExp e ++ ";"]
    ++ map (indent 2) (concatMap genStmt body)
    ++ ["}"]
  LRead s v ->
       [ "printf(\"" ++ s ++ "\\n\");"
       , "scanf(\"%d\", &" ++ pretty v ++ ");"
       ]
  LPrint e ->
       case e of
         LVal (VStr s) -> ["printf(\"" ++ s ++ "\\n\");"]
         _             -> ["printf(\"%d\\n\", " ++ goExp e ++ ");"]
  LAssign v e ->
       ["int " ++ pretty v ++ " = " ++ goExp e ++ ";"]
  where
    indent n = (replicate (n*2) ' ' ++)

goExp :: E2 -> String
goExp = \case
  LVal (VInt n) -> show n
  LVal (VStr s) -> '"' : s ++ "\""
  LVar v        -> pretty v
  LAdd  a b     -> bin "+" a b
  LMinus a b    -> bin "-" a b
  LMul  a b     -> bin "*" a b
  LDiv  a b     -> bin "/" a b
 where
  bin op a b = "(" ++ goExp a ++ " " ++ op ++ " " ++ goExp b ++ ")"
