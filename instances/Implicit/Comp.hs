{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}

module Implicit.Comp
where

import Control.Effect.Implicit

newtype CompM ops a = CompM {
  unCompM 
    :: forall eff
     . (EffConstraint ops eff)
    => eff a
} deriving (Functor)

instance Applicative (CompM ops) where
  pure x = CompM $ pure x
  (CompM f) <*> (CompM g) = CompM $ f <*> g

instance Monad (CompM ops) where
  return x = CompM $ return x
  (CompM mx) >>= cont = CompM $
    mx >>= (unCompM . cont)