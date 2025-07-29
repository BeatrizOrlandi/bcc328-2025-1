module L.L2.Interpreter.Interp where

import L.L2.Frontend.Syntax
import Utils.Var
import Utils.Value
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Control.Monad (foldM)

-- envMut  : variáveis mutáveis (parte de L1)
-- envImm  : pilha de mapas imutáveis (topo = cabeça)

type Mut = Map Var Value
type Imm = [Map Var Value]

runL2 :: L2 -> IO ()
runL2 (L2 ss) = do
  _ <- interpStmts [] M.empty ss
  pure ()

-- interpretar lista de comandos
interpStmts :: Imm -> Mut -> [S2] -> IO (Imm,Mut)
interpStmts i m []     = pure (i,m)
interpStmts i m (s:ss) = do
  (i',m') <- interpS i m s
  interpStmts i' m' ss

-- interpretar um comando
interpS :: Imm -> Mut -> S2 -> IO (Imm,Mut)
interpS imm mut cmd = case cmd of
  LRead prompt v -> do
      putStrLn prompt
      val <- VInt . read <$> getLine
      pure (imm, M.insert v val mut)

  LPrint e -> do
      case evalE imm mut e of
        Left err -> putStrLn err
        Right v  -> putStrLn (show v)
      pure (imm,mut)

  LAssign v e -> case evalE imm mut e of
      Left err -> putStrLn err >> pure (imm,mut)
      Right val-> pure (imm, M.insert v val mut)

  -- Def bloco
  Def v e body -> case evalE imm mut e of
      Left err -> putStrLn err >> pure (imm,mut)
      Right val -> do
        -- adiciona escopo
        let newImm = M.singleton v val : imm
        (_,mut') <- interpStmts newImm mut body
        pure (imm,mut')         -- sai do escopo imutável

-- avaliação de expressão
evalE :: Imm -> Mut -> E2 -> Either String Value
evalE _ _ (LVal v) = Right v
evalE imm mut (LVar v) =
  case lookupImm v imm of
    Just val -> Right val
    Nothing  -> maybe (Left $ "Undefined var "++show v) Right (M.lookup v mut)
evalE i m (LAdd a b)   = binOp i m a b (.+.)
evalE i m (LMinus a b) = binOp i m a b minusOp
  where
    minusOp (VInt x) (VInt y) = Right (VInt (x - y))
    minusOp v1 v2             = Left $ unwords ["Type error on:", show v1, "-", show v2]

evalE i m (LMul a b)   = binOp i m a b (.*.)
evalE i m (LDiv a b)   = binOp i m a b divOp
  where divOp (VInt x) (VInt y) | y /= 0 = Right (VInt (x `div` y))
        divOp _ _ = Left "Division error"

binOp i m a b f = do
  v1 <- evalE i m a; v2 <- evalE i m b; f v1 v2

lookupImm :: Var -> Imm -> Maybe Value
lookupImm _ [] = Nothing
lookupImm v (m:ms) = case M.lookup v m of
                       Just x  -> Just x
                       Nothing -> lookupImm v ms
