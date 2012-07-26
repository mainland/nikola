{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Data.Array.Nikola.Backend.C.Monad
-- Copyright   : (c) Geoffrey Mainland 2012
-- License     : BSD-style
--
-- Maintainer  : Geoffrey Mainland <mainland@apeiron.net>
-- Stability   : experimental
-- Portability : non-portable

module Data.Array.Nikola.Backend.C.Monad (
    Context(..),

    PtrCExp(..),
    CExp(..),

    Idx(..),
    idxInit,
    idxStride,

    CudaDim(..),
    cudaDimVar,

    C(..),
    CEnv(..),
    runC,

    getFlags,

    getContext,
    inContext,
    ifHostContext,
    ifKernelContext,

    useIndex,
    getIndices,

    lookupVarTrans,
    extendVarTrans,

    gensym,

    addInclude,
    addTypedef,
    addPrototype,
    addGlobal,
    addParam,
    addLocal,
    addStm,
    addFinalStm,

    inBlock,
    inNewBlock,
    inNewBlock_,
    inNewFunction
  ) where

import Control.Applicative (Applicative,
                            (<$>))
import Control.Monad.Exception
import Control.Monad.State
import Data.List (foldl')
import qualified Data.Map as Map
import qualified Data.Set as Set
import Language.C.Quote.CUDA
import qualified Language.C.Syntax as C
import Text.PrettyPrint.Mainland

#if !MIN_VERSION_template_haskell(2,7,0)
import qualified Data.Loc
import qualified Data.Symbol
import qualified Language.C.Syntax
#endif /* !MIN_VERSION_template_haskell(2,7,0) */

import Data.Array.Nikola.Backend.Flags
import Data.Array.Nikola.Language.Check
import Data.Array.Nikola.Language.Syntax

-- | Code generation context
data Context = Host
             | Kernel
  deriving (Eq, Ord, Show)

-- | C representation of Nikola array data
data PtrCExp = PtrCE C.Exp
             | TupPtrCE [PtrCExp]

-- | C representation of Nikola values
data CExp = VoidCE
          | ScalarCE C.Exp
          | TupCE [CExp]
          | ArrayCE PtrCExp [CExp]
          | FunCE C.Exp

instance Pretty PtrCExp where
    ppr (PtrCE ce)     = ppr ce
    ppr (TupPtrCE ces) = tuple (map ppr ces)

instance Pretty CExp where
    ppr VoidCE            = text "void"
    ppr (ScalarCE ce)     = ppr ce
    ppr (TupCE ces)       = tuple (map ppr ces)
    ppr (ArrayCE ce dims) = ppr ce <> brackets (commasep (map ppr dims))
    ppr (FunCE   ce)      = ppr ce

instance ToExp PtrCExp where
    toExp (PtrCE ce) = toExp ce
    toExp ce         = faildoc $ text "Cannot convert" <+>
                                 (squotes . ppr) ce <+>
                                 text "to a C expression"

instance ToExp CExp where
    toExp (ScalarCE ce)  = toExp ce
    toExp (ArrayCE ce _) = toExp ce
    toExp (FunCE ce)     = toExp ce

    toExp ce = faildoc $ text "Cannot convert" <+>
                         (squotes . ppr) ce <+>
                         text "to a C expression"

instance MonadCheck C where
    lookupVarType v = do
        maybe_tau <- gets $ \s -> Map.lookup v (cVarTypes s)
        case maybe_tau of
          Just tau -> return tau
          Nothing  -> faildoc $ text "Variable" <+> ppr v <+>
                      text "not in scope"

    extendVarTypes vtaus act = do
        old_cVarTypes <- gets cVarTypes
        modify $ \s -> s { cVarTypes = foldl' insert (cVarTypes s) vtaus }
        x  <- act
        modify $ \s -> s { cVarTypes = old_cVarTypes }
        return x
      where
        insert m (k, v) = Map.insert k v m

-- Loop index variable
data Idx = CIdx
         | CudaThreadIdx CudaDim
  deriving (Eq, Ord, Show)

idxInit :: Idx -> C.Exp
idxInit CIdx = [cexp|0|]
idxInit (CudaThreadIdx dim) =
    [cexp|blockIdx.$id:x*blockDim.$id:x + threadIdx.$id:x|]
  where
    x = cudaDimVar dim

idxStride :: Idx -> String -> C.Exp
idxStride CIdx v = [cexp|++$id:v|]
idxStride (CudaThreadIdx dim) v =
    [cexp|$id:v += blockDim.$id:x*gridDim.$id:x|]
  where
    x = cudaDimVar dim

data CudaDim = CudaDimX
             | CudaDimY
             | CudaDimZ
  deriving (Eq, Ord, Show)

cudaDimVar :: CudaDim -> String
cudaDimVar CudaDimX = "x"
cudaDimVar CudaDimY = "y"
cudaDimVar CudaDimZ = "z"

-- The state used by the C code generation monad
data CEnv = CEnv
  {  cFlags :: Flags

  ,  cVarTypes :: Map.Map Var Type
  ,  cVarTrans :: Map.Map Var CExp

  ,  cUniq :: !Int

  ,  cContext :: Context

  ,  cIndices :: [(Idx, Exp)]

  ,  cIncludes   :: Set.Set String
  ,  cTypedefs   :: [C.Definition]
  ,  cPrototypes :: [C.Definition]
  ,  cGlobals    :: [C.Definition]

  ,  cParams    :: [C.Param]
  ,  cLocals    :: [C.InitGroup]
  ,  cStms      :: [C.Stm]
  ,  cFinalStms :: [C.Stm]
  }

emptyCEnv :: Flags -> CEnv
emptyCEnv flags = CEnv
  {  cFlags = flags

  ,  cVarTypes = Map.empty
  ,  cVarTrans = Map.empty

  ,  cUniq = 0

  ,  cContext = Host

  ,  cIndices = []

  ,  cIncludes   = Set.empty
  ,  cTypedefs   = []
  ,  cPrototypes = []
  ,  cGlobals    = []

  ,  cParams    = []
  ,  cLocals    = []
  ,  cStms      = []
  ,  cFinalStms = []
  }

newtype C a = C { unC :: StateT CEnv (ExceptionT IO) a }
  deriving (Functor,
            Applicative,
            Monad,
            MonadException,
            MonadIO,
            MonadState CEnv)

runC :: Flags -> C a -> IO (a, [C.Definition])
runC flags m = do
    (a, env) <- runExceptionT (runStateT (unC m) (emptyCEnv flags)) >>= liftException
    return (a, envToCUnit env)
  where
    envToCUnit :: CEnv -> [C.Definition]
    envToCUnit env =
        [cunit|$edecls:includes
               $edecls:typedefs
               $edecls:prototypes
               $edecls:globals|]
      where
        includes = map toInclude (Set.toList (cIncludes env))
          where
            toInclude :: String -> C.Definition
            toInclude inc = [cedecl|$esc:("#include " ++ inc)|]

        typedefs   = (reverse . cTypedefs) env
        prototypes = (reverse . cPrototypes) env
        globals    = (reverse . cGlobals) env

getFlags :: C Flags
getFlags = gets cFlags

getContext :: C Context
getContext =
    gets cContext

inContext :: Context -> C a -> C a
inContext ctx act = do
    old_varTypes <- gets cVarTypes
    old_ctx      <- gets cContext
    modify $ \s -> s { cContext = ctx  }
    when (ctx == Kernel) $
        modify $ \s -> s { cVarTypes = Map.filter isFunT (cVarTypes s) }
    a <- act
    modify $ \s -> s { cVarTypes = old_varTypes
                     , cContext  = old_ctx
                     }
    return a

ifHostContext :: C () -> C ()
ifHostContext act = do
    ctx <- gets cContext
    when (ctx == Host) act

ifKernelContext :: C () -> C ()
ifKernelContext act = do
    ctx <- gets cContext
    when (ctx == Kernel) act

useIndex :: (Idx, Exp) -> C ()
useIndex idx =
    modify $ \s -> s { cIndices = idx : cIndices s }

getIndices :: C [(Idx, Exp)]
getIndices = do
    idxs <- gets cIndices
    modify $ \s -> s { cIndices = [] }
    return idxs

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

gensym :: String -> C String
gensym s = do
    u <- gets cUniq
    modify $ \s -> s { cUniq = u + 1 }
    return $ s ++ show u

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

addFinalStm :: C.Stm -> C ()
addFinalStm def = modify $ \s ->
    s { cFinalStms = def : cFinalStms s }

inBlock :: C a -> C a
inBlock act = do
    (a, items) <- inNewBlock act
    addStm [cstm|{ $items:items }|]
    return a

inNewBlock :: C a -> C (a, [C.BlockItem])
inNewBlock act = do
    oldCLocals    <- gets cLocals
    oldCStms      <- gets cStms
    oldCFinalStms <- gets cFinalStms
    modify $ \s -> s { cLocals = [], cStms = [], cFinalStms = [] }
    x <- act
    locals    <- reverse <$> gets cLocals
    stms      <- reverse <$> gets cStms
    finalstms <- gets cFinalStms
    modify $ \s -> s { cLocals    = oldCLocals
                     , cStms      = oldCStms
                     , cFinalStms = oldCFinalStms
                     }
    return (x, map C.BlockDecl locals ++
               map C.BlockStm stms ++
               map C.BlockStm finalstms)

inNewBlock_ :: C () -> C [C.BlockItem]
inNewBlock_ act = snd <$> inNewBlock act

inNewFunction :: C () -> C ([C.Param], [C.BlockItem])
inNewFunction comp = do
    oldCParams <- gets cParams
    modify $ \s -> s { cParams = [] }
    (_, items) <- inNewBlock comp
    params <- gets cParams
    modify $ \s -> s { cParams = oldCParams }
    return (reverse params, items)
