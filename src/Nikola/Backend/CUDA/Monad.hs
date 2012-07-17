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

module Nikola.Backend.CUDA.Monad (
    C,
    runC,

    CExp(..),
    BlockVar(..),
    GridVar(..),
    DevAlloc(..),
    ExecConfig(..),

    addSymbol,
    gensym,
    addInclude,
    addTypedef,
    addPrototype,
    addGlobal,
    addParam,
    addLocal,
    addStm,
    inNewBlock,
    inNewBlock_,
    inNewFunction,
    getExecConfig,
    withBlockVar,
    Ctx(..),
    nestCtx,
    withGridVar,
    insertNTrans,
    lookupNTrans,
    lookupVarTrans,
    extendVarTrans,
    inNewCompiledFunction,
    addDevAlloc,
    getDevAllocs
  ) where

import Control.Applicative
import Control.Monad.Exception
import Control.Monad.State
import Data.List (foldl')
import qualified Data.Map as Map
import qualified Data.Set as Set
import Language.C.Quote.C
import qualified Language.C.Syntax as C
import Text.PrettyPrint.Mainland

#if !MIN_VERSION_template_haskell(2,7,0)
import qualified Data.Loc
import qualified Data.Symbol
import qualified Language.C.Syntax
#endif /* !MIN_VERSION_template_haskell(2,7,0) */

import Nikola.Check
import Nikola.Syntax

-- | A compiled expression.
data CExp = ScalarCExp C.Exp
          | VectorCExp C.Exp C.Exp
          | MatrixCExp C.Exp N N N
          | FunCExp String Tau [DevAlloc] [DevAlloc]

instance Pretty CExp where
    ppr (ScalarCExp e)       = ppr e
    ppr (VectorCExp e _)     = ppr e
    ppr (MatrixCExp e _ _ _) = ppr e
    ppr (FunCExp v _ _ _)    = ppr [cexp|$id:v|]

instance ToExp CExp where
    toExp (ScalarCExp e)       = const e
    toExp (VectorCExp e _)     = const e
    toExp (MatrixCExp e _ _ _) = const e
    toExp (FunCExp v _ _ _)    = const [cexp|$id:v|]

-- | Index into a thread block
data BlockVar = BlockVar { blockDim   :: Int
                         , blockWidth :: Int
                         }
  deriving (Eq, Ord, Show)

instance Pretty BlockVar where
    ppr = text . show

instance ToExp BlockVar where
    toExp bv _ = go (blockDim bv)
      where
        go :: Int -> C.Exp
        go 0 = [cexp|threadIdx.x|]
        go 1 = [cexp|threadIdx.y|]
        go 2 = [cexp|threadIdx.z|]
        go _ = error "bad thread block variable"

-- | Index into a grid
data GridVar = GridVar { gridDim      :: Int
                       , gridBlockVar :: BlockVar
                       }
  deriving (Eq, Ord, Show)

instance Pretty GridVar where
    ppr = text . show

instance ToExp GridVar where
    toExp (GridVar { gridBlockVar = t }) _ =
        [cexp|(blockIdx.x + blockIdx.y*gridDim.x)*$(blockWidth t) + $t|]

-- | An on-device allocation needed by a compiled expression. We may need to
-- allocate device memory to hold intermediate results or to hold the final
-- result of a computation.
data DevAlloc = DevAlloc
    {  devAllocVar    :: String
    ,  devAllocParams :: [C.Param]
    ,  devAllocType   :: Tau
    }

-- The state used by the CUDA code generation monad
data CEnv = CEnv
  {  cVarTypes :: Map.Map Var Tau
  ,  cVarTrans :: Map.Map Var CExp

  ,  cNTrans   :: Map.Map N C.Exp

  ,  cSymbols :: Set.Set String

  ,  cIncludes   :: Set.Set String
  ,  cTypedefs   :: [C.Definition]
  ,  cPrototypes :: [C.Definition]
  ,  cGlobals    :: [C.Definition]

  ,  cParams :: [C.Param]
  ,  cLocals :: [C.InitGroup]
  ,  cStms   :: [C.Stm]

  ,  cDevAllocs     :: [DevAlloc]
  ,  cGridVars      :: Map.Map N GridVar
  ,  cBlockVars     :: [BlockVar]
  ,  cUsedBlockVars :: Set.Set BlockVar
  }

emptyCEnv :: CEnv
emptyCEnv = CEnv
  {  cVarTypes = Map.empty
  ,  cVarTrans = Map.empty

  ,  cNTrans = Map.empty

  ,  cSymbols = Set.empty

  ,  cIncludes   = Set.empty
  ,  cTypedefs   = []
  ,  cPrototypes = []
  ,  cGlobals    = []

  ,  cParams = []
  ,  cLocals = []
  ,  cStms   = []

  ,  cDevAllocs     = []
  ,  cGridVars      = Map.empty
  ,  cBlockVars     = []
  ,  cUsedBlockVars = Set.empty
  }

newtype C a = C { unC :: StateT CEnv (ExceptionT IO) a }
  deriving (Functor,
            Applicative,
            Monad,
            MonadException,
            MonadIO,
            MonadState CEnv)

runC :: C a -> IO (a, [C.Definition])
runC m = do
    (a, env) <- runExceptionT (runStateT (unC m) emptyCEnv) >>= liftException
    return (a, envToCUnit env)
  where
    envToCUnit :: CEnv -> [C.Definition]
    envToCUnit env =
        [cunit|$edecls:includes
        $edecls:typedefs
        $edecls:prototypes
        $edecls:globals
        |]
      where
        includes = map toInclude (Set.toList (cIncludes env))
          where
            toInclude :: String -> C.Definition
            toInclude inc =
                [cedecl|$esc:inc'|]
              where
                inc' = "#include " ++ inc

        typedefs   = (reverse . cTypedefs) env
        prototypes = (reverse . cPrototypes) env
        globals    = (reverse . cGlobals) env

instance MonadCheck C where
    lookupVar v = do
        maybe_tau <- gets $ \s -> Map.lookup v (cVarTypes s)
        case maybe_tau of
          Just tau -> return tau
          Nothing -> faildoc $ text "Variable" <+> ppr v <+>
                     text "not in scope"

    extendVars vtaus act = do
        old_cVarTypes <- gets cVarTypes
        modify $ \s -> s { cVarTypes = foldl' insert (cVarTypes s) vtaus }
        x  <- act
        modify $ \s -> s { cVarTypes = old_cVarTypes }
        return x
      where
        insert m (k, v) = Map.insert k v m

addSymbol :: String -> C ()
addSymbol sym = modify $ \s ->
    s { cSymbols = Set.insert sym (cSymbols s) }

gensym :: String -> C String
gensym str = do
    syms     <- gets cSymbols
    let str' =  head [s | s <- ss, s `Set.notMember` syms]
    modify $ \s -> s { cSymbols = Set.insert str' (cSymbols s) }
    return str'
  where
    ss = str : [str ++ show i | i <- [0 :: Integer ..]]

addInclude :: String -> C ()
addInclude inc = modify $ \s ->
    s { cIncludes = Set.insert inc (cIncludes s) }

addTypedef :: C.Definition -> C ()
addTypedef def = modify $ \s ->
    s { cTypedefs = def : cTypedefs s }

addPrototype :: C.Definition -> C ()
addPrototype def = modify $ \s ->
    s { cPrototypes = def : cPrototypes s }

addGlobal :: C.Definition -> C ()
addGlobal def = modify $ \s ->
    s { cGlobals = def : cGlobals s }

addParam :: C.Param -> C ()
addParam param = modify $ \s ->
    s { cParams = param : cParams s }

addLocal :: C.InitGroup -> C ()
addLocal def = modify $ \s ->
    s { cLocals = def : cLocals s }

addStm :: C.Stm -> C ()
addStm def = modify $ \s ->
    s { cStms = def : cStms s }

inNewBlock :: C a -> C (a, [C.BlockItem])
inNewBlock act = do
    oldCSymbols <- gets cSymbols
    oldCLocals  <- gets cLocals
    oldCStms    <- gets cStms
    modify $ \s -> s { cLocals = [], cStms = [] }
    x <- act
    locals <- reverse <$> gets cLocals
    stms   <- reverse <$> gets cStms
    modify $ \s -> s { cSymbols = oldCSymbols
                            , cLocals = oldCLocals
                            , cStms = oldCStms
                            }
    return (x, map C.BlockDecl locals ++ map C.BlockStm stms)

inNewBlock_ :: C () -> C [C.BlockItem]
inNewBlock_ act = snd <$> inNewBlock act

inNewFunction :: C a -> C (a, [C.Param], [C.BlockItem])
inNewFunction comp = do
    oldCParams <- gets cParams
    modify $ \s -> s { cParams = [] }
    (x, items) <- inNewBlock comp
    params <- gets cParams
    modify $ \s -> s { cParams = oldCParams }
    return (x, reverse params, items)

getExecConfig :: C ExecConfig
getExecConfig = do
    gridVars         <- gets (Map.toList . cGridVars)
    blockVars        <- gets cBlockVars
    usedBlockVars    <- gets cUsedBlockVars
    let (dimX, dimY) =  gridDims gridVars
    return ExecConfig { gridDimX  = dimX
                      , gridDimY  = dimY
                      , blockDimX = blockDim blockVars usedBlockVars 0
                      , blockDimY = blockDim blockVars usedBlockVars 1
                      , blockDimZ = blockDim blockVars usedBlockVars 2
                      }
  where
    blockDim :: [BlockVar] -> Set.Set BlockVar -> Int -> Int
    blockDim blockVars usedBlockVars i
        | length blockVars > i  && t `Set.member` usedBlockVars = blockWidth t
        | otherwise                                             = 1
      where
        t = blockVars !! i

    gridDims :: [(N, GridVar)] -> (N, N)
    gridDims ((n, g) : _) =
        (nGridDimX n w, nGridDimY n w)
      where
        w = (blockWidth . gridBlockVar) g

    gridDims _ = (1, 1)

withBlockVar :: Ctx -> (Maybe BlockVar -> C a) -> C a
withBlockVar (TopFun _) cont = do
    blockVars <- gets cBlockVars
    case blockVars of
      v : vs -> do  modify $ \s ->
                      s { cBlockVars     = vs
                        , cUsedBlockVars = Set.insert v (cUsedBlockVars s)
                        }
                    x <- cont (Just v)
                    modify $ \s -> s { cBlockVars = v : vs }
                    return x
      [] -> cont Nothing

withBlockVar _ cont = cont Nothing

-- | Context in which compilation occurs.
data Ctx = TopFun    { level :: Int }
         | NestedFun { level :: Int }

nestCtx :: Ctx -> Ctx
nestCtx ctx = ctx { level = level ctx + 1 }

withGridVar :: Ctx -> N -> (Maybe GridVar -> C a) -> C a
withGridVar (TopFun 0) n cont = do
    maybe_g <- gets (Map.lookup n . cGridVars)
    case maybe_g of
      Just g -> cont (Just g)
      Nothing -> newGridVar >>= cont
  where
    newGridVar :: C (Maybe GridVar)
    newGridVar = do
        ngridVars <- gets (Map.size . cGridVars)
        case ngridVars of
          0 -> withBlockVar (TopFun 0) $ \maybe_t ->
               case maybe_t of
                 Nothing -> return Nothing
                 Just t -> do  let g = GridVar { gridDim = 0
                                               , gridBlockVar = t
                                               }
                               modify $ \s ->
                                   s { cGridVars = Map.insert n g
                                                   (cGridVars s) }
                               return (Just g)
          _ -> return Nothing

withGridVar _ _ cont = cont Nothing

insertNTrans :: N -> C.Exp -> C ()
insertNTrans n e = modify $ \s ->
    s { cNTrans = Map.insert n e (cNTrans s) }

lookupNTrans :: N -> C (Maybe C.Exp)
lookupNTrans n =
    gets $ \s -> Map.lookup n (cNTrans s)

lookupVarTrans :: Var -> C CExp
lookupVarTrans v = do
    maybe_cexp <- gets $ \s -> Map.lookup v (cVarTrans s)
    case maybe_cexp of
      Just cexp -> return cexp
      Nothing ->   faildoc $ text "Variable" <+> ppr v <+> text "not in scope"

extendVarTrans :: [(Var, CExp)] -> C a -> C a
extendVarTrans vexps act = do
    old_cVarTrans <- gets cVarTrans
    modify $ \s -> s { cVarTrans = foldl' insert (cVarTrans s) vexps }
    x  <- act
    modify $ \s -> s { cVarTrans = old_cVarTrans }
    return x
  where
    insert m (k, v) = Map.insert k v m

inNewCompiledFunction :: C a -> C a
inNewCompiledFunction comp = do
    old_cDevAllocs <- gets cDevAllocs
    old_cNTrans    <- gets cNTrans
    modify $ \s -> s { cDevAllocs  = []
                     , cNTrans     = Map.empty
                     }
    x <- comp
    modify $ \s -> s { cDevAllocs  = old_cDevAllocs
                     , cNTrans     = old_cNTrans
                     }
    return x

addDevAlloc :: DevAlloc -> C ()
addDevAlloc alloc = modify $ \s ->
    s { cDevAllocs = alloc : cDevAllocs s }

getDevAllocs :: C [DevAlloc]
getDevAllocs = gets cDevAllocs
