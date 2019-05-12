{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Implicit.Stateful
where

import Implicit.Comp
import Control.Effect.Implicit
import Control.Effect.Implicit.Transform.State
import Control.Effect.Implicit.Ops.State
  ( StateEff
  )

import Control.Monad.Identity

import qualified Control.Effect.Implicit.Ops.State as ST

type StateM = CompM (StateEff Int)

get :: StateM Int
get = CompM ST.get

put :: Int -> StateM ()
put x = CompM $ ST.put x

runStateful :: forall a . Int -> StateM a -> (Int, a)
runStateful s (CompM comp) =
  runIdentity $
  withStateTAndOps @NoEff s $
  do
    x <- comp
    s' <- ST.get
    return (s', x)
