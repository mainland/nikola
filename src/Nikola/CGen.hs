-- Copyright (c) 2010-2012
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
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Nikola.CGen (
    CGenEnv,
    emptyCGenEnv,
    MonadCGen(..),
    codeToUnit
  ) where

import Control.Applicative
import qualified Data.Set as Set
import Language.C.Quote.C
import qualified Language.C.Syntax as C

#if !MIN_VERSION_template_haskell(2,7,0)
import qualified Data.Loc
import qualified Data.Symbol
import qualified Language.C.Syntax
#endif /* !MIN_VERSION_template_haskell(2,7,0) */

data CGenEnv = CGenEnv
  {  cSymbols :: Set.Set String

  ,  cIncludes   :: Set.Set String
  ,  cTypedefs   :: [C.Definition]
  ,  cPrototypes :: [C.Definition]
  ,  cGlobals    :: [C.Definition]

  ,  cParams :: [C.Param]
  ,  cLocals :: [C.InitGroup]
  ,  cStms   :: [C.Stm]
  }

emptyCGenEnv :: CGenEnv
emptyCGenEnv = CGenEnv
  {  cSymbols = Set.empty

  ,  cIncludes   = Set.empty
  ,  cTypedefs   = []
  ,  cPrototypes = []
  ,  cGlobals    = []

  ,  cParams = []
  ,  cLocals = []
  ,  cStms   = []
  }

codeToUnit :: CGenEnv -> [C.Definition]
codeToUnit code =
    [cunit|$edecls:includes
    $edecls:typedefs
    $edecls:prototypes
    $edecls:globals
    |]
  where
    includes = map toInclude (Set.toList (cIncludes code))
      where
        toInclude :: String -> C.Definition
        toInclude inc =
            [cedecl|$esc:inc'|]
          where
            inc' = "#include " ++ inc

    typedefs   = (reverse . cTypedefs) code
    prototypes = (reverse . cPrototypes) code
    globals    = (reverse . cGlobals) code

class (Functor m, Monad m) => MonadCGen m where
    getCGenEnv :: m CGenEnv
    putCGenEnv :: CGenEnv -> m ()

    getsCGenEnv :: (CGenEnv -> a) -> m a
    getsCGenEnv f = getCGenEnv >>= \s -> return (f s)

    modifyCGenEnv :: (CGenEnv -> CGenEnv) -> m ()
    modifyCGenEnv f = getCGenEnv >>= \s -> putCGenEnv (f s)

    addSymbol :: String -> m ()
    addSymbol sym = modifyCGenEnv $ \s ->
        s { cSymbols = Set.insert sym (cSymbols s) }

    gensym :: String -> m String
    gensym str = do
        syms     <- getsCGenEnv cSymbols
        let str' =  head [s | s <- ss, s `Set.notMember` syms]
        modifyCGenEnv $ \s -> s { cSymbols = Set.insert str' (cSymbols s) }
        return str'
      where
        ss = str : [str ++ show i | i <- [0 :: Integer ..]]

    addInclude :: String -> m ()
    addInclude inc = modifyCGenEnv $ \s ->
        s { cIncludes = Set.insert inc (cIncludes s) }

    addTypedef :: C.Definition -> m ()
    addTypedef def = modifyCGenEnv $ \s ->
        s { cTypedefs = def : cTypedefs s }

    addPrototype :: C.Definition -> m ()
    addPrototype def = modifyCGenEnv $ \s ->
        s { cPrototypes = def : cPrototypes s }

    addGlobal :: C.Definition -> m ()
    addGlobal def = modifyCGenEnv $ \s ->
        s { cGlobals = def : cGlobals s }

    addParam :: C.Param -> m ()
    addParam param = modifyCGenEnv $ \s ->
        s { cParams = param : cParams s }

    addLocal :: C.InitGroup -> m ()
    addLocal def = modifyCGenEnv $ \s ->
        s { cLocals = def : cLocals s }

    addStm :: C.Stm -> m ()
    addStm def = modifyCGenEnv $ \s ->
        s { cStms = def : cStms s }

    inNewBlock :: m a -> m (a, [C.BlockItem])
    inNewBlock act = do
        oldCSymbols <- getsCGenEnv cSymbols
        oldCLocals  <- getsCGenEnv cLocals
        oldCStms    <- getsCGenEnv cStms
        modifyCGenEnv $ \s -> s { cLocals = [], cStms = [] }
        x <- act
        locals <- reverse <$> getsCGenEnv cLocals
        stms   <- reverse <$> getsCGenEnv cStms
        modifyCGenEnv $ \s -> s { cSymbols = oldCSymbols
                                , cLocals = oldCLocals
                                , cStms = oldCStms
                                }
        return (x, map C.BlockDecl locals ++ map C.BlockStm stms)

    inNewBlock_ :: m () -> m [C.BlockItem]
    inNewBlock_ act = snd <$> inNewBlock act

    inNewFunction :: m a -> m (a, [C.Param], [C.BlockItem])
    inNewFunction comp = do
        oldCParams <- getsCGenEnv cParams
        modifyCGenEnv $ \s -> s { cParams = [] }
        (x, items) <- inNewBlock comp
        params <- getsCGenEnv cParams
        modifyCGenEnv $ \s -> s { cParams = oldCParams }
        return (x, reverse params, items)
