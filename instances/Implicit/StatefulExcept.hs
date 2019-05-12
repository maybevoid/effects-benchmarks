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

import qualified Control.Effect.Implicit.Ops.State as ST

type StateM = CompM (StateEff Int ∪ ExceptionEff String)

get :: StateM Int
get = CompM ST.get

put :: Int -> StateM ()
put x = CompM $ ST.put x

throw :: String -> StateM a
throw e = CompM $ raise e

runStatefulExcept :: forall a . Int -> StateM a -> Either String (Int, a)
runStatefulExcept s (CompM comp1) = comp6
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
    => Computation (StateEff Int ∪ ExceptionEff String) (Return (Int, a)) eff
  comp3 = genericReturn comp2

  comp4 :: forall eff . (Effect eff)
    => Computation (ExceptionEff String) (Return (Int, a)) eff
  comp4 = runPipelineWithCast cast cast
    (stateTPipeline s)
    comp3

  errHandler1 :: forall eff . (Effect eff)
    => CoOpHandler (ExceptionEff String) (Int, a) (Either String (Int, a)) eff
  errHandler1 = exceptionToEitherHandler

  errHandler2 :: forall eff . (Effect eff)
    => Pipeline
        NoEff
        (ExceptionEff String)
        (Return (Int, a))
        (Return (Either String (Int, a)))
        eff eff
  errHandler2 = coopHandlerToPipeline @FreeMonad $ genericComputation errHandler1

  comp5 :: forall eff . (Effect eff)
    => Computation NoEff (Return (Either String (Int, a))) eff
  comp5 = runPipelineWithCast cast cast
    errHandler2 comp4

  comp6 :: Either String (Int, a)
  comp6 = runIdentityComp comp5
