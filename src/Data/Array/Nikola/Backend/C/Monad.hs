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
    PtrCExp(..),
    CExp(..),

    Idx(..),

    CudaDim(..),
    cudaDimVar,

    CudaKernel(..),
    CudaThreadBlockDim,
    CudaGridDim,

    CudaWorkBlock(..),

    C(..),
    CEnv(..),
    defaultCEnv,
    runC,
    cenvToCUnit,

    getFlags,

    useIndex,
    collectIndices,
    getIndices,

    addWorkBlock,
    collectWorkBlocks,
    getWorkBlocks,

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
    inNewFunction,
    collectDefinitions
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

-- ^ Loop index variable
data Idx = CIdx
         | CudaThreadIdx CudaDim
         | IrregCudaThreadIdx CudaDim
  deriving (Eq, Ord, Show)

-- ^ CUDA dimension
data CudaDim = CudaDimX
             | CudaDimY
             | CudaDimZ
  deriving (Eq, Ord, Show)

cudaDimVar :: CudaDim -> String
cudaDimVar CudaDimX = "x"
cudaDimVar CudaDimY = "y"
cudaDimVar CudaDimZ = "z"

-- ^ CUDA kernel
data CudaKernel = CudaKernel
    { cukernDefs ::  [C.Definition]  -- ^ The kernel's compiled C source
    , cukernName :: String           -- ^ The name of the kernel
    , cukernIdxs :: [(Idx, Exp)]     -- ^ A list of indices the kernel uses and
                                     -- their bounds. The bounds are written in
                                     -- terms of the kernel's parameters.

    , cukernWorkBlocks :: [CudaWorkBlock] -- ^ The kernel's work blocks.

    , cukernRewrite :: Exp -> [Exp] -> Exp -- ^ Rewrite an expression written in
                                           -- terms of the kernel's parameters
                                           -- to an expression written in terms
                                           -- of a list of arguments.
    }

type CudaThreadBlockDim = (Int, Int, Int)

type CudaGridDim = (Int, Int, Int)

data CudaWorkBlock = CudaWorkBlock
    { cuworkBlockCounter :: String -- ^ The name of the CUDA global that holds
                                   -- the number of processed blocks.
    }

-- The state used by the C code generation monad
data CEnv = CEnv
  {  cFlags :: Flags

  ,  cVarTypes :: Map.Map Var Type
  ,  cVarTrans :: Map.Map Var CExp

  ,  cUniq :: !Int

  ,  cContext :: Context

  ,  cIndices :: [(Idx, Exp)]

  ,  cWorkBlocks :: [CudaWorkBlock]

  ,  cIncludes   :: Set.Set String
  ,  cTypedefs   :: [C.Definition]
  ,  cPrototypes :: [C.Definition]
  ,  cGlobals    :: [C.Definition]

  ,  cParams    :: [C.Param]
  ,  cLocals    :: [C.InitGroup]
  ,  cStms      :: [C.Stm]
  ,  cFinalStms :: [C.Stm]
  }

defaultCEnv :: Flags -> CEnv
defaultCEnv flags = CEnv
  {  cFlags = flags

  ,  cVarTypes = Map.empty
  ,  cVarTrans = Map.empty

  ,  cUniq = 0

  ,  cContext = Host

  ,  cIndices = []

  ,  cWorkBlocks = []

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

runC :: C a -> CEnv -> IO (a, CEnv)
runC m s = do
    (a, s') <- runExceptionT (runStateT (unC m) s) >>= liftException
    return (a, s')

cenvToCUnit :: CEnv -> [C.Definition]
cenvToCUnit env =
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

instance MonadCheck C where
    getContext = gets cContext

    setContext ctx = modify $ \s -> s { cContext = ctx }

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

getFlags :: C Flags
getFlags = gets cFlags

useIndex :: (Idx, Exp) -> C ()
useIndex idx =
    modify $ \s -> s { cIndices = idx : cIndices s }

collectIndices :: C a -> C (a, [(Idx, Exp)])
collectIndices act = do
    old_idxs <- gets cIndices
    modify $ \s -> s { cIndices = [] }
    a    <- act
    idxs <- gets cIndices
    modify $ \s -> s { cIndices = old_idxs }
    return (a, reverse idxs)

getIndices :: C [(Idx, Exp)]
getIndices =
    reverse <$> gets cIndices

addWorkBlock :: CudaWorkBlock -> C ()
addWorkBlock wb =
    modify $ \s -> s { cWorkBlocks = wb : cWorkBlocks s }

collectWorkBlocks :: C a -> C (a, [CudaWorkBlock])
collectWorkBlocks act = do
    old_wbs <- gets cWorkBlocks
    modify $ \s -> s { cWorkBlocks = [] }
    a   <- act
    wbs <- gets cWorkBlocks
    modify $ \s -> s { cWorkBlocks = old_wbs }
    return (a, wbs)

getWorkBlocks :: C [CudaWorkBlock]
getWorkBlocks =
    gets cWorkBlocks

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

collectDefinitions :: C a -> C (a, [C.Definition])
collectDefinitions act = do
    old_cIncludes   <- gets cIncludes
    old_cTypedefs   <- gets cTypedefs
    old_cPrototypes <- gets cPrototypes
    old_cGlobals    <- gets cGlobals
    modify $ \s -> s {  cIncludes   = Set.empty
                     ,  cTypedefs   = []
                     ,  cPrototypes = []
                     ,  cGlobals    = []
                     }
    a    <- act
    s'   <- get
    modify $ \s -> s {  cIncludes   = old_cIncludes `Set.union` cIncludes s'
                     ,  cTypedefs   = old_cTypedefs   ++ cTypedefs s'
                     ,  cPrototypes = old_cPrototypes ++ cPrototypes s'
                     ,  cGlobals    = old_cGlobals    ++ cGlobals s'
                     }
    return (a, cenvToCUnit s')
