{-# LANGUAGE LambdaCase #-}

module L.L2.Frontend.TypeCheck (typeCheck) where

import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.State
import           L.L2.Frontend.Syntax
import           Utils.Var                   (Var(..))

------------------------------------------------------------
-- Ambiente e monad (já vinham no esqueleto)
------------------------------------------------------------

type TcM a = ExceptT String (StateT TcEnv Identity) a

data TcEnv = TcEnv { context :: [Var] }
initTcEnv :: TcEnv
initTcEnv = TcEnv []

insertVar, removeVar :: Var -> TcM ()
insertVar v = modify $ \e -> e{context = v : context e}
removeVar v = modify $ \e -> e{context = filter (/= v) (context e)}

isImmutable :: Var -> TcM Bool
isImmutable v = gets (elem v . context)

runTcM :: TcM a -> Either String a
runTcM m = runIdentity $ evalStateT (runExceptT m) initTcEnv

------------------------------------------------------------
-- Algoritmo de verificação
------------------------------------------------------------

typeCheck :: L2 -> Either String L2
typeCheck prog@(L2 ss) = runTcM (mapM_ checkStmt ss) >> pure prog

checkStmt :: S2 -> TcM ()
checkStmt = \case
  LAssign v _e -> do
      isImmutable v >>= \case
         True  -> throwError $ "atribuição a variável imutável: " ++ show v
         False -> pure ()
  LRead _ v    -> checkStmt (LAssign v (LVal (error "dummy"))) -- mesma regra
  LPrint e     -> checkExp e
  Def v e body -> do
      checkExp e
      insertVar v
      mapM_ checkStmt body
      removeVar v

checkExp :: E2 -> TcM ()
checkExp = \case
  LVal _       -> pure ()
  LVar _       -> pure ()
  LAdd a b     -> check a b
  LMinus a b   -> check a b
  LMul a b     -> check a b
  LDiv a b     -> check a b
  where
    check a b = checkExp a >> checkExp b
