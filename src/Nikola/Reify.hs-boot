module Nikola.Reify where

import Control.Monad.State

data REnv

newtype R a = R { unR :: StateT REnv IO a }
