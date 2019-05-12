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
import Control.Effect.Implicit.Transform.State
import Control.Effect.Implicit.Ops.Exception
import Control.Effect.Implicit.Ops.State (StateEff)

import Control.Monad.Identity
import qualified Control.Effect.Implicit.Ops.State as ST

type StateM = CompM (StateEff Int ∪ ExceptionEff String)

get :: StateM Int
get = CompM ST.get

put :: Int -> StateM ()
put x = CompM $ ST.put x

throw :: String -> StateM a
throw e = CompM $ raise e

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
  comp3 =
    withCoOpHandlerAndOps @ChurchMonad @NoEff exceptionToEitherHandler $
    withStateTAndOps @(ExceptionEff String) s $
    comp2
