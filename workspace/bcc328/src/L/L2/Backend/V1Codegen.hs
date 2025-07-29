{-# LANGUAGE LambdaCase #-}

module L.L2.Backend.V1Codegen where

import L.L2.Frontend.Syntax
import Utils.Value
import V.V1.Instr

v1Codegen :: L2 -> Code
v1Codegen (L2 ss2) =
  (concatMap s2Codegen ss2) ++ [Halt]

s2Codegen :: S2 -> Code
s2Codegen (Def v e s) =
  e2Codegen e
    ++ [Store v]
    ++ concatMap s2Codegen s
s2Codegen (LRead s v) =
  [ Push (VStr s)
  , Print
  , Input
  , Store v
  ]
s2Codegen (LPrint e2) =
  e2Codegen e2 ++ [Print]
s2Codegen (LAssign v e2) =
  e2Codegen e2 ++ [Store v]

e2Codegen :: E2 -> Code
e2Codegen (LVal v) = [Push v]
e2Codegen (LVar v) = [Load v]
e2Codegen (LAdd l0 l1) =
  e2Codegen l0 ++ e2Codegen l1 ++ [Add]
e2Codegen (LMinus l0 l1) =
  e2Codegen l0 ++ e2Codegen l1 ++ [Sub]
e2Codegen (LMul l0 l1) =
  e2Codegen l0 ++ e2Codegen l1 ++ [Mul]
e2Codegen (LDiv l0 l1) =
  e2Codegen l0 ++ e2Codegen l1 ++ [Div]
