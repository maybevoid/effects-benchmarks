{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ImplicitParams #-}

module Implicit.HTTP
where

import Implicit.Comp
import Control.Effect.Implicit

data HttpEff

data HttpOps eff = HttpOps
  { openOp :: String -> eff ()
  , closeOp :: eff ()
  , postOp :: String -> eff String
  , getOp :: eff String
  }

instance EffFunctor HttpOps where
  effmap lift ops = HttpOps
    { openOp = lift . openOp ops
    , closeOp = lift $ closeOp ops
    , postOp = lift . postOp ops
    , getOp = lift $ getOp ops
    }

instance EffOps HttpEff where
  type Operation HttpEff = HttpOps

instance ImplicitOps HttpEff where
  type OpsConstraint HttpEff eff =
    (?httpOps :: HttpOps eff)

  withOps ops comp = let ?httpOps = ops in comp
  captureOps = ?httpOps

type HttpM = CompM HttpEff

open' :: String -> HttpM ()
open' x = CompM $ openOp captureOps x

close' :: HttpM ()
close' = CompM $ closeOp captureOps

post' :: String -> HttpM String
post' x = CompM $ postOp captureOps x

get' :: HttpM String
get' = CompM $ getOp captureOps

dummyOps
  :: forall eff
   . (Effect eff)
  => HttpOps eff
dummyOps = HttpOps
  { openOp = \_ -> return ()
  , closeOp = return ()
  , postOp = \x ->
      return $ "posted" <> x
  , getOp = return "gotten"
}

runHttp :: HttpM a -> IO a
runHttp (CompM comp) = withOps dummyOps comp

