{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Implicit.StatefulExcept
where

import Implicit.Comp
import Control.Effect.Implicit
import Control.Effect.Implicit.Ops.Exception
import Control.Effect.Implicit.Ops.State (StateEff, StateCoOp (..))

import Control.Monad.Identity
import qualified Control.Effect.Implicit.Ops.State as ST

type StateM = CompM (StateEff Int ∪ ExceptionEff String)

get :: StateM Int
get = CompM ST.get

put :: Int -> StateM ()
put x = CompM $ ST.put x

throw :: String -> StateM a
throw e = CompM $ raise e

stateExceptionHandler
  :: forall eff s e a
   . (Effect eff)
  => CoOpHandler (StateEff s ∪ ExceptionEff e) a (s -> eff (Either e a)) eff
stateExceptionHandler = CoOpHandler handleReturn handleOps
 where
  handleReturn :: a -> eff (s -> eff (Either e a))
  handleReturn x = return $ \_ -> return $ Right x

  handleOps
    :: UnionCoOp
        (StateCoOp s)
        (ExceptionCoOp e)
        (eff (s -> eff (Either e a)))
    -> eff (s -> eff (Either e a))
  handleOps (LeftCoOp (GetOp cont1)) = return $
    \s -> do
      cont2 <- cont1 s
      cont2 s

  handleOps (LeftCoOp (PutOp s cont1)) = return $
    \_ -> do
      cont2 <- cont1 ()
      cont2 s

  handleOps (RightCoOp (RaiseOp e)) = return $
    \_ -> return $ Left e

runStatefulExcept :: forall a . Int -> StateM a -> Either String (Int, a)
runStatefulExcept s (CompM comp1) = runIdentity comp3
 where
  comp2
    :: forall eff
     . (EffConstraint (StateEff Int ∪ ExceptionEff String) eff)
    => eff (Int, a)
  comp2 = do
    x <- comp1
    s' <- ST.get
    return (s', x)

  comp3 :: forall eff . (Effect eff)
    => eff (Either String (Int, a))
  comp3 = do
    comp4 <- withCoOpHandlerAndOps
      @ChurchMonad @NoEff stateExceptionHandler $
      comp2
    comp4 s
