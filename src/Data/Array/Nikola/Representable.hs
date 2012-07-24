-- Copyright (c) 2009-2012
--         The President and Fellows of Harvard College.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
-- 3. Neither the name of the University nor the names of its contributors
--    may be used to endorse or promote products derived from this software
--    without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE UNIVERSITY AND CONTRIBUTORS ``AS IS'' AND
-- ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
-- ARE DISCLAIMED.  IN NO EVENT SHALL THE UNIVERSITY OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
-- OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
-- LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
-- OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
-- SUCH DAMAGE.

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Array.Nikola.Representable (
    Representable(..),
    Elt,
    IsVector
  ) where

import Data.Typeable
import Foreign.Storable

import Data.Array.Nikola.Language.Syntax

-- | 'Representable t a' means that 'a' can be represented on target 't'.
class (Typeable t, Typeable a, Storable (Rep a)) => Representable t a where
    -- | 'CallCtx t' is a monad for constructing the call context for the target
    -- 't'.
    type CallCtx t :: * -> *

    -- | 'Rep a' is the /representation/ type for 'a' when a value of type 'a'
    -- is tranferred to the target.
    type Rep a :: *

    -- | Convert to representation type.
    toRep :: t -> a -> IO (Rep a)

    -- | Convert from representation type.
    fromRep :: t -> Rep a -> IO a

    -- | The embedded type that corresponds to 'a'.
    embeddedType :: t -> a -> ParamIdx -> Tau

    -- | Extend the current execution context with an argument of type 'a' and
    -- then continue by performing an action in the call context.
    withArg :: t -> a -> CallCtx t b -> CallCtx t b

    -- | An action in the call context monad that returns a result of type 'a'.
    returnResult :: t -> CallCtx t a

-- | A scalar type that can be used in GPU code. This is used to constrain the
-- set of legal types that can appear in containers, e.g., vectors and matrices.
class Representable t a => Elt t a where

-- | The class of vectors when embedded in an 'Exp'.
class Representable t (v a) => IsVector t v a where

