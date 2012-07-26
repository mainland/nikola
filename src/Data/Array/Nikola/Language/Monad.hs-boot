module Data.Array.Nikola.Language.Monad (
    REnv,
    R
  ) where

data REnv

newtype R r a = R { unR :: REnv -> (REnv -> a -> IO r) -> IO r }
