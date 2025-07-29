{-# LANGUAGE LambdaCase #-}

module L.L2.Backend.V1Codegen (v1Codegen) where

import L.L2.Frontend.Syntax
import Utils.Pretty (pretty)
import Utils.Value  (Value(..))

v1Codegen :: L2 -> [String]
v1Codegen (L2 ss) = concatMap genStmt ss

genStmt :: S2 -> [String]
genStmt = \case
  LAssign v e     -> genExp e ++ ["STORE " ++ pretty v]
  LRead msg v     -> ["PUSH_STR \"" ++ msg ++ "\""
                     ,"READ"
                     ,"STORE " ++ pretty v]
  LPrint e        -> genExp e ++ ["PRINT"]
  Def v e body    ->
       genExp e ++ ["PUSH_IMMUT " ++ pretty v]
    ++ concatMap genStmt body
    ++ ["POP_IMMUT " ++ pretty v]

genExp :: E2 -> [String]
genExp = \case
  LVal (VInt n) -> ["PUSH_INT " ++ show n]
  LVal (VStr s) -> ["PUSH_STR \"" ++ s ++ "\""]
  LVar v        -> ["LOAD " ++ pretty v]
  LAdd a b      -> genExp a ++ genExp b ++ ["ADD"]
  LMinus a b    -> genExp a ++ genExp b ++ ["SUB"]
  LMul a b      -> genExp a ++ genExp b ++ ["MUL"]
  LDiv a b      -> genExp a ++ genExp b ++ ["DIV"]
