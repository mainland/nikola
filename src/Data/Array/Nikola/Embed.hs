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

module Data.Array.Nikola.Embed (
    Representable(..),
    Elt,
    IsVector,

    Exp(..),

    Reifiable,
    reify,
    reifyEx,

    ReifiableFun,
    reifyLam,

    VApply,
    vapply
  ) where

import Control.Applicative
import Control.Monad.Trans (liftIO)
import Data.Typeable

import Data.Array.Nikola.Language.Check
import Data.Array.Nikola.Language.Reify
import Data.Array.Nikola.Language.Reify.Monad
import Data.Array.Nikola.Language.Syntax

-- | 'Representable t a' means that 'a' can be represented on target 't'.
class (Typeable t, Typeable a) => Representable t a where
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

-- | A wrapping of the core 'DExp' type that provides a phantom type parameter.
newtype Exp t a = E { unE :: DExp }
  deriving (Show, Typeable)

instance Eq (Exp t a) where
    _ == _ = error "Cannot compare values of type Exp a"

instance Ord (Exp t a) where
    _ `compare` _ = error "Cannot compare values of type Exp a"

-- | The @Reifiable@ class tells us what types can be reified to a @DExp@.
class Reifiable a where
    reify :: a -> IO DExp
    reify = reifyEx defaultROpts

    reifyEx :: ROpts -> a -> IO DExp

-- @DExp@'s and @Exp@'s are reified by running them through @reifyE@.
instance Reifiable DExp where
    reifyEx ropts e = runR ropts (reifyE e return)

instance Reifiable (Exp t a) where
    reifyEx ropts = reifyEx ropts . unE

-- Functions that are instances of @ReifiableFun@ are reified by calling
-- @reifyFun@.
instance ReifiableFun a b => Reifiable (a -> b) where
    reifyEx ropts f = runR ropts (reifyFun f >>= reifyBareLamE)

reifyLam :: ReifiableFun a b
         => (a -> b)
         -> (DExp -> R DExp)
         -> R DExp
reifyLam f kont = do
    e <- reifyFun f
    reifyE e kont

-- | @reifyFun f kont@ reifies the function @f@ and passes the parameters and
-- body of the reified function to @kont@
class (Typeable a, Typeable b) => ReifiableFun a b where
    reifyFun :: (a -> b) -> R DExp

-- The next two instances represent the base cases for function
-- reification. They reify a function by gensym'ing a fresh variable name,
-- passing it to the function, and then reifying the result by calling @reifyE@.
instance (Representable t a,
          Representable t b) => ReifiableFun (Exp t a) (Exp t b) where
    reifyFun f = do
        v          <- gensym "x"
        let fOfX   =  (unE . f . E) (VarE v)
        let tau    =  embeddedType (undefined :: t) (undefined :: a) (ParamIdx 0)
        extendVars [(v, tau)] $ do
        return $ shiftLamE v tau fOfX

instance (Representable t a,
          Representable t b) => ReifiableFun (Exp t a) (IO (Exp t b)) where
    reifyFun f = do
        v          <- gensym "x"
        fOfX       <- liftIO $ unE <$> (f . E) (VarE v)
        let tau    =  embeddedType (undefined :: t) (undefined :: a) (ParamIdx 0)
        extendVars [(v, tau)] $ do
        return $ shiftLamE v tau fOfX

-- This is the inductive case. As with the base cases, we gensym a fresh
-- variable and pass it to @f@ to yield @g@. We reify @g@---itself a
-- function---by calling @reifyFun@.
instance (Representable t a,
          ReifiableFun b c) => ReifiableFun (Exp t a) (b -> c) where
    reifyFun f = do
        v       <- gensym "x"
        let tau =  embeddedType (undefined :: t) (undefined :: a) (ParamIdx 0)
        let g   =  f (E (VarE v))
        extendVars [(v, tau)] $ do
        shiftLamE v tau <$> reifyFun g

-- | @vapply@ is a bit tricky... We first build a @DelayedE@ AST node containing
-- an action that reifies the lambda. Then we wrap the result in enough
-- (Haskell) lambdas and (Nikola) @AppE@ constructors to turn in back into a
-- Haskell function (at the original type) whose body is a Nikola application
-- term.
class (ReifiableFun a b) => VApply a b where
    vapply :: (a -> b) -> a -> b
    vapply f = vapplyk (DelayedE (cacheDExp f (reifyLam f))) []

    vapplyk :: DExp -> [DExp] -> a -> b

instance (Representable t a,
          Representable t b) => VApply (Exp t a) (Exp t b) where
    vapplyk f es = \e -> E $ AppE f (reverse (unE e : es))

instance (Representable t a,
          VApply b c) => VApply (Exp t a) (b -> c) where
    vapplyk f es = \e -> vapplyk f (unE e : es)
