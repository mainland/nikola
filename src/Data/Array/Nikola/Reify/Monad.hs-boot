module Data.Array.Nikola.Reify.Monad (
    REnv,
    R
  ) where

import Control.Monad.State

data REnv

newtype R a = R { unR :: StateT REnv IO a }
