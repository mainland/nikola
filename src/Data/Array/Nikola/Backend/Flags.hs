-- |
-- Module      : Data.Array.Nikola.Backend.Flags
-- Copyright   : (c) Geoffrey Mainland 2012
-- License     : BSD-style
--
-- Maintainer  : Geoffrey Mainland <mainland@apeiron.net>
-- Stability   : experimental
-- Portability : non-portable

module Data.Array.Nikola.Backend.Flags (
    Dialect(..),
    Flags(..),
    defaultFlags,

    ljust,
    fromLJust
  ) where

import Data.Function (on)
import Data.Monoid (Monoid(..), Last(..))
import Text.PrettyPrint.Mainland

data Dialect = SequentialC
             | OpenMP
             | CUDA
             | OpenCL
  deriving (Eq, Ord, Enum, Show)

instance Pretty Dialect where
    ppr = text . show

data Flags = Flags
    { fDialect    :: Last Dialect  -- ^ The language dialect to which we compile.
    , fOptimize   :: Last Int      -- ^ Optimization level
    , fVerbosity  :: Last Int      -- ^ Verbosity level
    , fObsSharing :: Last Bool     -- ^ Observer sharing
    , fPprCols    :: Last Int      -- ^ Number of columns for pretty printing
    , fFunction   :: Last String   -- ^ Name of function being compiled
    , fOutput     :: Last FilePath -- ^ Filename of output.
    , fHelp       :: Last Bool     -- ^ Print help and exit.
    } deriving (Eq, Show)

instance Monoid Flags where
    mempty  = emptyFlags
    mappend = appendFlags

ljust :: a -> Last a
ljust = Last . Just

fromLJust :: (Flags -> Last a)
          -> Flags
          -> a
fromLJust fld f = case fld f of
                    Last Nothing  -> fromLJust fld defaultFlags
                    Last (Just a) -> a

defaultFlags :: Flags
defaultFlags = Flags
    { fDialect    = ljust CUDA
    , fOptimize   = ljust 0
    , fVerbosity  = ljust 0
    , fObsSharing = ljust True
    , fPprCols    = ljust 80
    , fFunction   = mempty
    , fOutput     = mempty
    , fHelp       = ljust False
    }

emptyFlags :: Flags
emptyFlags = Flags
    { fDialect    = mempty
    , fOptimize   = mempty
    , fVerbosity  = mempty
    , fObsSharing = mempty
    , fPprCols    = mempty
    , fFunction   = mempty
    , fOutput     = mempty
    , fHelp       = mempty
    }

appendFlags :: Flags -> Flags -> Flags
appendFlags a b = Flags
    { fDialect    = app fDialect a b
    , fOptimize   = app fOptimize a b
    , fVerbosity  = app fVerbosity a b
    , fObsSharing = app fObsSharing a b
    , fPprCols    = app fPprCols a b
    , fFunction   = app fFunction a b
    , fOutput     = app fOutput a b
    , fHelp       = app fHelp a b
    }
  where
    app f = mappend `on` f
