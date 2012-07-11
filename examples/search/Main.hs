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
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Prelude hiding (catch)

import CGen

import Control.Applicative
import Control.Exception
import Control.Monad.Logic
import Control.Monad.State
import Data.Generics.Uniplate.Data
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Vector as Vector
import qualified Foreign.CUDA.Driver as CU
import Language.C.Quote.CUDA
import Language.C.Smart
import qualified Language.C.Syntax as C
import Numeric.Container
import Statistics.Sample
import System.Console.GetOpt
import System.Environment
import System.IO
import Text.PrettyPrint.Mainland
import Text.Printf

#if !MIN_VERSION_template_haskell(2,7,0)
import qualified Data.Loc
import qualified Data.Symbol
import qualified Language.C.Syntax
#endif /* !MIN_VERSION_template_haskell(2,7,0) */

import Nikola hiding (map)
import Nikola.Embeddable.Hmatrix ()

data GridVar = GridVar { gridDimIdx :: Int
                       , gridStrips :: Int
                       }
  deriving (Eq, Ord, Show)

data BlockVar = BlockVar { blockDimIdx :: Int
                         , blockWidth  :: Int
                         }
  deriving (Eq, Ord, Show)

data IVar = SerialIVar { ivarVar       :: String
                       , ivarUnrolling :: Int
                       }
          | ParIVar   { ivarVar        :: String
                      , ivarUnrolling  :: Int
                      , ivarGridVar    :: GridVar
                      , ivarBlockVar   :: Maybe BlockVar
                      }
  deriving (Eq, Ord, Show)

instance ToExp IVar where
    toExp ivar _ = [cexp|$id:(ivarVar ivar)|]

isSerialIVar :: IVar -> Bool
isSerialIVar (SerialIVar {}) = True
isSerialIVar _              = False

isBlockIVar :: IVar -> Bool
isBlockIVar (ParIVar {ivarBlockVar = Just _}) = True
isBlockIVar _                                 = False

blockIdx :: forall m . Monad m => IVar -> m C.Exp
blockIdx idx = go idx
  where
    go :: IVar -> m C.Exp
    go ivar@(ParIVar {}) = (toDim . gridDimIdx . ivarGridVar) ivar
    go _                 = fail "Not a parallelized index"

    toDim :: Int -> m C.Exp
    toDim 0 = return [cexp|blockIdx.x|]
    toDim 1 = return [cexp|blockIdx.y|]
    toDim 2 = return [cexp|blockIdx.z|]
    toDim _ = fail "blockIdx: bad index"

threadIdx :: forall m . Monad m => IVar -> m C.Exp
threadIdx idx = go idx
  where
    go :: IVar -> m C.Exp
    go (ParIVar {ivarBlockVar = Just blockVar}) =
        (toDim . blockDimIdx) blockVar
    go _ =
        fail "Not a thread block index"

    toDim :: Int -> m C.Exp
    toDim 0 = return [cexp|threadIdx.x|]
    toDim 1 = return [cexp|threadIdx.y|]
    toDim _ = fail "blockIdx: bad index"

ivarBlockWidth :: Monad m => IVar -> m Int
ivarBlockWidth (ParIVar {ivarBlockVar = Just blockVar}) =
    return (blockWidth blockVar)
ivarBlockWidth _ =
    fail "Not a thread block index"

ivarStripWidth :: Integral a => IVar -> a
ivarStripWidth (SerialIVar {}) = 1

ivarStripWidth (ParIVar { ivarGridVar  = GridVar { gridStrips = s }
                        , ivarBlockVar = Nothing
                        }) =
    fromIntegral s

ivarStripWidth (ParIVar { ivarGridVar  = GridVar { gridStrips = s }
                        , ivarBlockVar = Just blockVar
                        }) =
    fromIntegral (blockWidth blockVar * s)

data IVarEnv = IVarEnv
  {  nGridIndices  :: Int
  ,  nBlockIndices :: Int

  ,  ivars :: [IVar]
  }

emptyIVarState :: IVarEnv
emptyIVarState = IVarEnv
  {  nGridIndices  = 0
  ,  nBlockIndices = 0

  ,  ivars = []
  }

class (Functor m, MonadPlus m) => MonadIVar m where
    getIVarEnv :: m IVarEnv
    putIVarEnv :: IVarEnv -> m ()

    getsIVarEnv :: (IVarEnv -> a) -> m a
    getsIVarEnv f = getIVarEnv >>= \s -> return (f s)

    modifyIVarEnv :: (IVarEnv -> IVarEnv) -> m ()
    modifyIVarEnv f = getIVarEnv >>= \s -> putIVarEnv (f s)

    getIVars :: m [IVar]
    getIVars = getsIVarEnv ivars

    getIVar :: Int -> m IVar
    getIVar n = getsIVarEnv ((!! n) . ivars)

    newIVar :: Int -> [Int] -> m IVar
    newIVar strips widths =
        newParIVar `mplus` newSerialIVar
      where
        newSerialIVar :: m IVar
        newSerialIVar = do
            let ivar = SerialIVar { ivarVar       = error "ivar translation undefined"
                                  , ivarUnrolling = error "ivar unrolling undefined"
                                  }
            modifyIVarEnv $ \s -> s { ivars = ivars s ++ [ivar] }
            return ivar

        newParIVar :: m IVar
        newParIVar = do
            gridVar  <- newGridVar
            blockVar <- chooseM [return Nothing, Just <$> newBlockVar]
            let ivar = ParIVar { ivarVar       = error "ivar translation undefined"
                               , ivarUnrolling = error "ivar unrolling undefined"
                               , ivarGridVar   = gridVar
                               , ivarBlockVar  = blockVar
                               }
            modifyIVarEnv $ \s ->
                s { ivars = ivars s ++ [ivar] }
            return ivar

        newGridVar :: m GridVar
        newGridVar = do
            ng <- getsIVarEnv nGridIndices
            guard (ng < 3)
            modifyIVarEnv $ \s -> s { nGridIndices = ng + 1 }
            return $ GridVar { gridDimIdx = ng
                             , gridStrips = strips
                             }

        newBlockVar :: m BlockVar
        newBlockVar = do
            nt <- getsIVarEnv nBlockIndices
            w  <- choose widths
            guard (nt < 2)
            modifyIVarEnv $ \s -> s { nBlockIndices = nt + 1 }
            return $ BlockVar { blockDimIdx = nt
                              , blockWidth = w
                              }

type Metric = (Double, Double)

data Config = Config
    {  configIVars  :: (IVar, IVar)
    ,  configIUnroll :: Int
    ,  configJUnroll :: Int
    ,  configKUnroll :: Int
    }
  deriving (Eq, Ord)

instance Show Config where
    show (Config (x, y) i j k) =
        "k_" ++
         iv x ++ "x" ++ show (i+1) ++ "_" ++
         iv y ++ "x" ++ show (j+1) ++ "_" ++
         show (k+1)
      where
         iv :: IVar -> String
         iv (SerialIVar {}) =
             "p"

         iv ivar@(ParIVar { ivarBlockVar = Nothing }) =
             (show . gridStrips . ivarGridVar) ivar ++ "x1"

         iv ivar@(ParIVar { ivarBlockVar = Just blockVar }) =
             (show . gridStrips . ivarGridVar) ivar ++ "x" ++ (show . blockWidth) blockVar

data ConfigEnv = ConfigEnv
  { configRun :: Bool

  , configN :: Int
  , configA :: Matrix Float
  , configB :: Matrix Float
  , configC :: [[Float]]

  , configKernelConfig :: Config
  , configMetrics      :: Map.Map Config Metric
  }

emptyConfigEnv :: ConfigEnv
emptyConfigEnv = ConfigEnv
    { configRun = False

    , configN = n
    , configA = a
    , configB = b
    , configC = c

    , configKernelConfig = error "No configuration specified"
    , configMetrics      = Map.empty
    }
  where
    n :: Int
    n = 512

    a :: Matrix Float
    a = buildMatrix n n $ \(i, j) ->
        (fromIntegral i)*2.0 + (fromIntegral j)*2.0

    b :: Matrix Float
    b = buildMatrix n n $ \(i, j) ->
        if i == j - 1 then
            -1.0
        else if i == j + 1 then
            2.0
        else
            0.0

    c :: [[Float]]
    c = toLists (a `multiply` b)

class (Functor m, MonadIO m, MonadPlus m) => MonadConfig m where
    getConfigEnv :: m ConfigEnv
    putConfigEnv :: ConfigEnv -> m ()

    getsConfigEnv :: (ConfigEnv -> a) -> m a
    getsConfigEnv f = getConfigEnv >>= \s -> return (f s)

    modifyConfigEnv :: (ConfigEnv -> ConfigEnv) -> m ()
    modifyConfigEnv f = getConfigEnv >>= \s -> putConfigEnv (f s)

    runKernels :: m Bool
    runKernels = getsConfigEnv configRun

    getKernelConfig :: m Config
    getKernelConfig = getsConfigEnv configKernelConfig

    setKernelConfig :: Config -> m ()
    setKernelConfig config = modifyConfigEnv $ \s ->
        s { configKernelConfig = config }

    addMetric :: Metric -> m ()
    addMetric metric = do
        config <- getKernelConfig
        modifyConfigEnv $ \s ->
            s { configMetrics = Map.insert config metric (configMetrics s) }

    getMetrics :: m [(Config, Metric)]
    getMetrics =
        getsConfigEnv (Map.toList . configMetrics)

    checkKernel :: CFun (Exp (Matrix Float) -> Exp (Matrix Float) -> Exp (Matrix Float))
                -> m (Maybe (Double, Double))
    checkKernel k = do
        s <- getConfigEnv
        liftIO $ check (configN s)
                       (configA s)
                       (configB s)
                       (configC s)
                 `catch` \(e :: CU.CUDAException) -> print e >> return Nothing
      where
        check :: Int
              -> Matrix Float
              -> Matrix Float
              -> [[Float]]
              -> IO (Maybe (Double, Double))
        check n a b c = do
            samples <- liftIO $
                       withCompiledCFun k $ \f ->
                       replicateM 5 $
                       timeKernel f $ \f -> do
                       call f a b
            when (not (all (== c) (map (toLists . snd) samples))) $
                fail "Incorrect kernel"
            let gflops = Vector.fromList (map (toGFlops . fst) samples)
            return $ Just (mean gflops, stdDev gflops)
          where
            toGFlops :: Double -> Double
            toGFlops t = fromIntegral (n*n*n*2) / realToFrac t / 1e9

data PersistentState = PersistentState
  { configState :: ConfigEnv }

emptyPersistentState :: Bool -> PersistentState
emptyPersistentState run = PersistentState
  { configState = emptyConfigEnv { configRun = run } }

data PathState = PathState
  { cgenState :: CGenEnv
  , ivarState :: IVarEnv
  }

emptyPathState :: PathState
emptyPathState = PathState
  { cgenState = emptyCGenEnv
  , ivarState = emptyIVarState
  }

newtype S a = S { unS :: StateT PathState (LogicT (StateT PersistentState IO)) a }
    deriving (Monad, MonadPlus, MonadIO, MonadLogic)

instance MonadCGen S where
    getCGenEnv     = S $ gets cgenState
    putCGenEnv env = S $ modify $ \s -> s { cgenState = env }

instance MonadIVar S where
    getIVarEnv     = S $ gets ivarState
    putIVarEnv env = S $ modify $ \s -> s { ivarState = env }

instance MonadConfig S where
    getConfigEnv     = S $ lift $ lift $ gets configState
    putConfigEnv env = S $ lift $ lift $ modify $ \s -> s { configState = env }

instance Functor S where
    fmap = liftM

instance Applicative S where
    pure = return
    (<*>) = ap

instance Alternative S where
    empty = mzero
    (<|>) = mplus

observeS :: Bool -> S a -> IO a
observeS doRun m =
    evalStateT (observeT (evalStateT (unS m) emptyPathState))
               (emptyPersistentState doRun)

observeManyS :: Bool -> Int -> S a -> IO [a]
observeManyS doRun n m =
    evalStateT (observeManyT n (evalStateT (unS m) emptyPathState))
               (emptyPersistentState doRun)

observeAllS :: Bool -> S a -> IO [a]
observeAllS doRun m =
    evalStateT (observeAllT (evalStateT (unS m) emptyPathState))
               (emptyPersistentState doRun)

whenM :: Monad m => m Bool -> m () -> m ()
whenM test act = do
    flag <- test
    when flag act

choose :: MonadPlus m => [a] -> m a
choose []       = mzero
choose [x]      = return x
choose (x : xs) = return x `mplus` choose xs

chooseM :: MonadPlus m => [m a] -> m a
chooseM []       = mzero
chooseM [x]      = x
chooseM (x : xs) = x `mplus` chooseM xs

guardM :: MonadPlus m => m Bool -> m ()
guardM test = test >>= guard

blockDim :: Int -> S Int
blockDim idx = do
    ivars <- getIVars
    return $ loop ivars
  where
    loop :: [IVar] -> Int
    loop [] = 1

    loop (ParIVar {ivarBlockVar = Just (BlockVar {blockDimIdx = idx', blockWidth = w})} : _)
        | idx' == idx = w

    loop (_ : ivars) = loop ivars

guardBlockSize :: (Int -> Bool) -> S ()
guardBlockSize f = do
    dimX <- blockDim 0
    dimY <- blockDim 1
    dimZ <- blockDim 2
    guard $ f (dimX*dimY*dimZ)

guardSomeParallelization :: S ()
guardSomeParallelization = do
    ivars <- getIVars
    guard ((not . all isSerialIVar) ivars)

chooseBlockIVar :: MonadPlus m
                => [IVar]
                -> m IVar
chooseBlockIVar = choose . filter isBlockIVar

parfor :: forall a . String  -- ^ Suggested name of the index variable
       -> C.Type             -- ^ Index variable type
       -> C.Exp              -- ^ From
       -> C.Exp              -- ^ To
       -> Int                -- ^ Step
       -> [Int]              -- ^ SIMD width
       -> [Int]              -- ^ Number of times to unroll the loop
       -> (IVar -> S a)      -- ^ Continuation (passed the SIMD width, number of
                             -- times the loop was unrolled, the loop variable,
                             -- and the loop index)
       -> S a
parfor v ty from to step widths nus cont = do
    nroll <- choose nus
    ivar <- newIVar (nroll+1) widths
    go nroll ivar
  where
    go :: Int -> IVar -> S a
    go nroll ivar@(SerialIVar {})= do
        let ivar' = ivar { ivarVar = v
                         , ivarUnrolling = nroll
                         }
        (x, body) <- inNewBlock $
                     unrollLoop v nroll $
                     cont ivar'
        addStm [cstm|for ($ty:ty $id:v = $from; $id:v < $to; $id:v += $int:(step + nroll))
                         $stm:body |]
        return x

    go nroll ivar@(ParIVar { ivarBlockVar = Nothing }) = do
        let ivar' = ivar { ivarVar = v
                         , ivarUnrolling = nroll
                         }
        gdim <- blockIdx ivar
        case nroll + 1 of
          1 -> addLocal [cdecl|const $ty:ty $id:v = $gdim;|]
          n -> addLocal [cdecl|const $ty:ty $id:v = $gdim*$int:n;|]
        unrollLoop v nroll $ cont ivar'

    go nroll ivar@(ParIVar { ivarBlockVar = Just _ }) = do
        let ivar' = ivar { ivarVar = v
                         , ivarUnrolling = nroll
                         }
        gdim <- blockIdx ivar
        tdim <- threadIdx ivar
        w    <- ivarBlockWidth ivar
        case nroll of
          0 -> addLocal [cdecl|const $ty:ty $id:v = $gdim*$(w) + $tdim;|]
          _ -> addLocal [cdecl|const $ty:ty $id:v = $gdim*$((nroll+1) * w) +
                                                    $int:(nroll+1)*$tdim;|]
        unrollLoop v nroll $ cont ivar'

for :: String         -- ^ Suggested name of the index variable
    -> C.Type         -- ^ Index variable type
    -> C.Exp          -- ^ From
    -> C.Exp          -- ^ To
    -> C.Exp          -- ^ Step
    -> [Int]          -- ^ Number of times to unroll the loop
    -> (IVar -> S a)  -- ^ Continuation (passed the loop index)
    -> S a
for v ty from to step nus cont = do
    nroll    <- choose nus
    let ivar = SerialIVar { ivarVar = v
                          , ivarUnrolling = nroll
                          }
    (x, body) <- inNewBlock $
                 unrollLoop v nroll $
                 cont ivar
    addStm [cstm|for ($ty:ty $id:v = $from; $id:v < $to; $id:v += $step)
                     $stm:body |]
    return x

nestfor :: (String, String)       -- ^ Suggested name of the index variable
        -> (C.Exp, C.Exp)         -- ^ To
        -> [Int]                  -- ^ SIMD width
        -> [Int]                  -- ^ Number of times to unroll the loop
        -> ((IVar, IVar) -> S a)  -- ^ Continuation (passed the SIMD width,
                                  -- number of times the loop was unrolled, the
                                  -- loop variable, and the loop index)
        -> S a
nestfor (iv, jv) (m, n) ws nus cont =
    parfor iv [cty|int|] 0 m 1 ws nus $ \i -> do
    parfor jv [cty|int|] 0 n 1 ws nus $ \j -> do
    cont (i, j)

data Flag = Run
  deriving (Eq)

options :: [OptDescr Flag]
options =
 [ Option ['r'] ["run"] (NoArg Run) "run kernels on GPU"
 ]

opts :: [String] -> IO ([Flag], [String])
opts argv =
   case getOpt Permute options argv of
      (o, n, []  ) -> return (o,n)
      (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where
    header = "Usage: search [OPTION...]..."

main :: IO ()
main = withNewContext $ \_ -> do
    (flags, _) <- System.Environment.getArgs >>= opts
    observeAllS (Run `elem` flags) (test innerloop1)
    -- observeS (Run `elem` flags) (test innerloop1)
    -- observeAllS (Run `elem` flags) (test innerloop2)
    -- observeS (Run `elem` flags) (test innerloop3)
    return ()

test :: S () -> S ()
test innerloop = do
    body <- inNewBlock_ innerloop
    addGlobal
      [cedecl|extern "C" __global__ void
              kmatmul(float *a, int lda, int m, int l,
                      float *b, int ldb, int l_, int n,
                      float *c)
              { $stm:body }|]
    (sx, wx, sy, wy) <- mkLaunch
    defs   <- getsCGenEnv codeToUnit
    let k :: CFun (Exp (Matrix Float) -> Exp (Matrix Float) -> Exp (Matrix Float))
        k = CFun { cfunName = "kmatmul"
                 , cfunDefs = defs
                 , cfunAllocs = [matrixT FloatT 1 0 1]
                 , cfunExecConfig = ExecConfig { gridDimX  = nGridDimX (NMatRows 0) sx
                                               , gridDimY  = nGridDimX (NMatCols 1) sy
                                               , blockDimX = wx
                                               , blockDimY = wy
                                               , blockDimZ = 1
                                               }
                 }
    desc <- show <$> getKernelConfig
    liftIO $ withFile (desc ++ ".cu") WriteMode $ \h ->
                 hPutStrLn h $ (show . stack . map ppr . cfunDefs) k
    run <- runKernels
    if run
      then do temp <- checkKernel k
              case temp of
                Just (mean, stddev) ->
                      liftIO $ printf "%s: %0.2f+/-%0.2f GFlops\n" desc mean stddev
                _ -> return ()
      else liftIO $ putStrLn $ (show . stack . map ppr . cfunDefs) k
  where
    gridDim :: Integral a => Int -> S (a -> a)
    gridDim idx = do
        w <- ivarStripWidth <$> getIVar idx
        return $ \n -> n `div` w

    mkLaunch :: S (Int, Int, Int, Int)
    mkLaunch = do
        dimX <- blockDim 0
        dimY <- blockDim 1
        dimZ <- blockDim 2
        strideX :: C.Exp <- ivarStripWidth <$> getIVar 0
        strideY :: C.Exp <- ivarStripWidth <$> getIVar 1
        gridDimX <- gridDim 0
        gridDimY <- gridDim 1
        -- config <- getKernelConfig
        -- liftIO $ print config
        -- liftIO $ print (dimX, dimY, dimZ, gridDimX 512, gridDimY 512)
        let cGridDimX = gridDimX [cexp|m|]
        let cGridDimY = gridDimY [cexp|n|]
        addInclude "<assert.h>"
        addGlobal
          [cedecl|extern "C" void
                  matmul(float *a, int lda, int m, int k,
                         float *b, int ldb, int k_, int n,
                         float *c)
                  { dim3 dimBlock;
                    dim3 dimGrid;
                    dimBlock.x = $int:dimX;
                    dimBlock.y = $int:dimY;
                    dimBlock.z = $int:dimZ;
                    dimGrid.x = $cGridDimX;
                    dimGrid.y = $cGridDimY;
                    dimGrid.z = 1;
                    assert(m % $strideX == 0);
                    assert(n % $strideY == 0);
                    kmatmul<<<dimGrid, dimBlock>>>(a, lda, m, k, b, ldb, k_, n, c);
                  }|]
        sx <- ivarStripWidth <$> getIVar 0
        wx <- blockDim 0
        sy <- ivarStripWidth <$> getIVar 1
        wy <- blockDim 1
        return (sx, wx, sy, wy)

innerloop1 :: S ()
innerloop1 = do
    parfor "i" [cty|int|] 0 m 1 ws nus $ \i -> do
    guard (not (isSerialIVar i))
    guard (ivarStripWidth i <= 512)
    parfor "j" [cty|int|] 0 n 1 ws nus $ \j -> do
        let iu =  ivarUnrolling i
        let ju =  ivarUnrolling j
        ku     <- choose nus
        guard (not (isSerialIVar j))
        guard (iu == 0 || ju == 0)
        guardSomeParallelization
        guardBlockSize (<= 512)
        guard (ivarStripWidth j <= 512)
        setKernelConfig (Config (i, j) iu ju ku)

        addLocal [cdecl|float sum = 0.0;|]
        temp <- choose [i, j]
        stripmine temp "k" [cty|int|] 0 l [ku] $ \k -> do
            addStm $ sum += a i k * b k j
        addStm $ c i j === sum
  where
    ws = [32, 64, 128, 256, 512]
    nus = [0, 1, 3, 7, 15]

    sum = [cexp|sum|]
    m = [cexp|m|]
    n = [cexp|n|]
    l = [cexp|l|]

    a :: (ToExp a, ToExp b) => a -> b -> C.Exp
    a i j = [cexp|a[$i+lda*$j]|]

    b :: (ToExp a, ToExp b) => a -> b -> C.Exp
    b i j = [cexp|b[$i+ldb*$j]|]

    c :: (ToExp a, ToExp b) => a -> b -> C.Exp
    c i j = [cexp|c[$i+ldb*$j]|]

innerloop2 :: S ()
innerloop2 =
    nestfor ("i", "j") (m, n) ws nus $ \(i, j) -> do
    guard (not (isSerialIVar i))
    guard (not (isSerialIVar j))
    guard (ivarStripWidth i <= 512)
    guard (ivarStripWidth j <= 512)
    guardBlockSize (<= 512)
    let iu =  ivarUnrolling i
    let ju =  ivarUnrolling j
    ku     <- choose nus
    guard (iu == 0 || ju == 0)
    setKernelConfig (Config (i, j) iu ju ku)

    addLocal [cdecl|float sum = 0.0;|]
    temp <- choose [i, j]
    stripmine temp "k" [cty|int|] 0 l [ku] $ \k -> do
        addStm $ sum += a i k * b k j
    addStm $ c i j === sum
  where
    ws = [32, 64, 128, 256, 512]
    nus = [0, 1, 3, 7, 15]

    sum = [cexp|sum|]
    m = [cexp|m|]
    n = [cexp|n|]
    l = [cexp|l|]

    a :: (ToExp a, ToExp b) => a -> b -> C.Exp
    a i j = [cexp|a[$i+lda*$j]|]

    b :: (ToExp a, ToExp b) => a -> b -> C.Exp
    b i j = [cexp|b[$i+ldb*$j]|]

    c :: (ToExp a, ToExp b) => a -> b -> C.Exp
    c i j = [cexp|c[$i+ldb*$j]|]

innerloop3 :: S ()
innerloop3 =
    nestfor ("i", "j") (m, n) ws nus $ \(i, j) -> do
    guard (isBlockIVar i)
    guard (isBlockIVar j)
    iw <- ivarBlockWidth i
    jw <- ivarBlockWidth j
    guard (ivarStripWidth i <= 512)
    guard (ivarStripWidth j <= 512)
    guardBlockSize (<= 512)
    let iu =  ivarUnrolling i
    let ju =  ivarUnrolling j
    ku     <- choose nus
    guard (iu == 0 || ju == 0)
    setKernelConfig (Config (i, j) iu ju ku)

    ix <- threadIdx i
    jx <- threadIdx j
    addLocal [cdecl|const int ix = $ix;|]
    addLocal [cdecl|const int jx = $jx;|]

    let ix = [cexp|ix|]
    let jx = [cexp|jx|]
    let kw = max iw jw

    let kw1 = case kw `mod` 16 of
                0 -> kw + 1
                _ -> kw
    let jw1 = case jw `mod` 16 of
                0 -> jw + 1
                _ -> jw

    addLocal [cdecl|__shared__ float at[$iw][$kw1];|]
    addLocal [cdecl|__shared__ float bt[$kw][$jw1];|]
    addLocal [cdecl|float sum = 0.0;|]

    for "ks" [cty|int|] 0 l [cexp|$kw|] [0] $ \ks -> do
        addStm [cstm|__syncthreads();|]
        unroll jx jw kw $ \k -> do
            addStm $ at ix k === a i [cexp|$ks + $k|]
        unroll ix iw kw $ \k -> do
            addStm $ bt k jx === b [cexp|$ks + $k|] j
        addStm [cstm|__syncthreads();|]

        for "k" [cty|int|] 0 [cexp|kw|] 1 [0] $ \k -> do
            addStm $ sum += at ix [cexp|$k|] * bt [cexp|$k|] jx
    addStm $ c i j === sum
  where
    ws = [16]
    nus = [0, 1, 3, 7, 15]

    unroll :: C.Exp
           -> Int
           -> Int
           -> (C.Exp -> S ())
           -> S ()
    unroll i iw jw cont
        | iw > jw   = fail "foo"
        | iw == jw  = cont i
        | otherwise = forM_ [0..n-1] $ \k ->
                      case k of
                        0 -> cont [cexp|$(n)*$i|]
                        _ -> cont [cexp|$(n)*$i+$(k)|]
      where
        n = jw `div` iw

    sum = [cexp|sum|]
    m = [cexp|m|]
    n = [cexp|n|]
    l = [cexp|l|]

    a :: (ToExp a, ToExp b) => a -> b -> C.Exp
    a i j = [cexp|a[$i+lda*$j]|]

    b :: (ToExp a, ToExp b) => a -> b -> C.Exp
    b i j = [cexp|b[$i+ldb*$j]|]

    c :: (ToExp a, ToExp b) => a -> b -> C.Exp
    c i j = [cexp|c[$i+ldb*$j]|]

    at :: (ToExp a, ToExp b) => a -> b -> C.Exp
    at i j = [cexp|at[$i][$j]|]

    bt :: (ToExp a, ToExp b) => a -> b -> C.Exp
    bt i j = [cexp|bt[$i][$j]|]

-- | Add the proper statments to synchronize threads after executing an
-- operation indexed by an @IVar@. When utilizing more threads than fit in a
-- single warp, we must call @__syncthreads()@. Otherwise, no synchronization is
-- needed.
syncthreads :: MonadCGen m
            => IVar -- ^ The @IVar@
            -> m ()
syncthreads (ParIVar { ivarBlockVar = Just t })
    | blockWidth t > 32 = addStm [cstm|__syncthreads();|]
syncthreads _ = return ()

-- | Strip-mine a loop of the form:
--
-- @
-- for (int i = 0; i < j; i += 1)
--     BODY
-- @
--
-- We look for an array access of the form @a[i + exp]@, where @i@ is the loop
-- variable, and then load a strip of @a@ into a shared array so that access is
-- sequential. @s@ is used to parallelize loading the strip into the shared
-- array.

stripmine :: IVar           -- ^ The index variable to use when strip-mining
          -> String         -- ^ Suggested name of the index variable
          -> C.Type         -- ^ Index variable type
          -> C.Exp          -- ^ From
          -> C.Exp          -- ^ To
          -> [Int]          -- ^ Number of times to unroll the loop
          -> (IVar -> S ()) -- ^ Continuation (passed the loop index)
          -> S ()
stripmine s v ty from to nus cont = do
    n        <- choose nus
    let ivar = SerialIVar { ivarVar = v
                          , ivarUnrolling = n
                          }
    body <- inNewBlock_ $
            cont ivar
    w <- ivarBlockWidth s
    t <- threadIdx s
    acc@[cexp|$id:arr[$id:_ + $idx]|] <- strideOneArrays v body
    addSymbol v
    addSymbol arr
    is     <- gensym (v ++ "s")
    arrbuf <- gensym (arr ++ "buf")
    addLocal [cdecl|__shared__ float $id:arrbuf[$int:w];|]
    for is ty from to [cexp|$int:w|] [0] $ \is -> do
        addStm [cstm|$id:arrbuf[$t] = $id:arr[$is + $t + $idx];|]
        syncthreads s
        for v ty [cexp|$is|] [cexp|$is + $int:w|] (fromIntegral (n+1)) [n] $ \i -> do
            addStm $ transformBi (replace acc [cexp|$id:arrbuf[$i-$is]|]) body
        syncthreads s
  where
    replace :: Eq a => a -> a -> a -> a
    replace from to = go
      where
        go x | x == from = to
        go x             = x

    strideOneArrays :: String -> C.Stm -> S C.Exp
    strideOneArrays i stm = do
        choose [e | e <- universeBi stm,
                    strideOneArray i e]
      where
        strideOneArray :: String -> C.Exp -> Bool
        strideOneArray i [cexp|$id:_[$id:i' + $e]|]
            | i' == i &&
              [cexp|$s|] `notElem` universeBi e = True
        strideOneArray _ _                      = False

type Phi = Set.Set String
type Theta = Map.Map String C.Exp

-- | Unroll the body of a loop of the form:
--
-- @
-- for (int i = 0; i < j; i += 1)
--     BODY
-- @
--
-- @n@ times. We assume that @j@ is divisible by @n@.
unrollLoop :: String
           -> Int
           -> S a
           -> S a
unrollLoop _ 0 act = act
unrollLoop i n act = do
    (x, stm) <- inNewBlock act
    stms <- unroll (Set.singleton i) (map initEnv [0..n]) stm
    addStm (toBlock stms)
    return x
  where
    initEnv :: Int -> Map.Map String C.Exp
    initEnv 0 = Map.singleton i [cexp|$id:i|]
    initEnv n = Map.singleton i [cexp|$id:i + $int:n|]

toBlock :: [C.Stm] -> C.Stm
toBlock []    = error "toBlock: empty statement list"
toBlock [stm] = stm
toBlock stms  = [cstm|{ $stms:stms }|]

containsOneOf :: (Ord b, Biplate a b)
              => a
              -> Set.Set b
              -> Bool
containsOneOf x vs = any (`Set.member` vs) (universeBi x)

dependsOn :: Biplate a C.Id
          => a
          -> Phi
          -> Bool
dependsOn x phi =
    any (`Set.member` phi) vs
  where
    vs = [v | C.Id v _ <- universeBi x]

subst :: Biplate a C.Exp
      => Theta
      -> a
      -> a
subst theta e = transformBi lookup e
  where
    lookup :: C.Exp -> C.Exp
    lookup e@[cexp|$id:v|] = case Map.lookup v theta of
                               Nothing -> e
                               Just e' -> e'
    lookup e               = e

class Unroll a b | a -> b where
    unroll :: Phi -> [Theta] -> a -> S b

instance Unroll C.Stm [C.Stm] where
    unroll phi thetas (C.Label id stm loc) = do
        (stm' : stms') <- unroll phi thetas stm
        return (C.Label id stm' loc : stms')

    unroll phi thetas (C.Case e stm loc) = do
        (stm') <- unroll phi thetas stm
        return ([C.Case e (toBlock stm') loc])

    unroll phi thetas (C.Default stm loc) = do
        (stm') <- unroll phi thetas stm
        return [C.Default (toBlock stm') loc]

    unroll _ _ (C.Exp Nothing _) =
        return []

    unroll phi thetas (C.Exp (Just e) loc) | dependsOn e phi = do
        return [C.Exp (Just (subst theta e)) loc | theta <- thetas]

    unroll _ _ (C.Exp (Just e) loc) = do
        return [C.Exp (Just e) loc]

    unroll phi thetas block@(C.Block items loc) = do
        (_, items') <- unrollBlockItems thetas items
        return [C.Block items' loc]
      where
        phi' :: Phi
        phi' = dependencies block phi

        unrollBlockItem :: [Theta] -> C.BlockItem -> S ([Theta], [C.BlockItem])
        unrollBlockItem thetas (C.BlockDecl init) = do
            (thetas', init') <- unrollInitGroup thetas init
            return (thetas', [C.BlockDecl init'])

        unrollBlockItem thetas (C.BlockStm stm) = do
            stms <- unroll phi' thetas stm
            return (thetas, map C.BlockStm stms)

        unrollBlockItem thetas item =
            return (thetas, [item])

        unrollBlockItems :: [Theta] -> [C.BlockItem] -> S ([Theta], [C.BlockItem])
        unrollBlockItems thetas [] = return (thetas, [])

        unrollBlockItems thetas (x : xs) = do
            (thetas', x')   <- unrollBlockItem thetas x
            (thetas'', xs') <- unrollBlockItems thetas' xs
            return (thetas'', x' ++ xs')

        unrollInit :: [Theta] -> C.Init -> S ([Theta], [C.Init])
        unrollInit thetas (C.Init (C.Id v vloc) decl label init attrs loc)
            | v `Set.member` phi' = do
            addSymbol v
            vs <- replicateM (length thetas) (gensym v)
            let thetas' = [Map.insert v [cexp|$id:v'|] theta | (theta, v') <- thetas `zip` vs]
            let init'   = [C.Init (C.Id v' vloc) decl label init attrs loc | v' <- vs]
            return (thetas', init')

        unrollInit thetas init =
            return (thetas, [init])

        unrollInits :: [Theta] -> [C.Init] -> S ([Theta], [C.Init])
        unrollInits thetas [] = return (thetas, [])

        unrollInits thetas (x : xs) = do
            (thetas', x')   <- unrollInit thetas x
            (thetas'', xs') <- unrollInits thetas' xs
            return (thetas'', x' ++ xs')

        unrollInitGroup :: [Theta] -> C.InitGroup -> S ([Theta], C.InitGroup)
        unrollInitGroup thetas (C.InitGroup dspec attrs inits loc) = do
            (thetas', inits') <- unrollInits thetas inits
            return (thetas', C.InitGroup dspec attrs inits' loc)

        unrollInitGroup thetas initGroup =
            return (thetas, initGroup)

    unroll phi thetas (C.If e th el loc) | dependsOn e phi =
        return stms
      where
        stms = [C.If (subst theta e) (subst theta th) (subst theta el) loc | theta <- thetas]

    unroll phi thetas (C.If e th el loc) = do
        th' <- unroll phi thetas th
        el' <- unroll phi thetas el
        return [C.If e (toBlock th') el' loc]

    unroll phi thetas (C.Switch e stm loc) | dependsOn e phi =
        return stms
      where
        stms = [C.Switch (subst theta e) (subst theta stm) loc | theta <- thetas]

    unroll phi thetas (C.Switch e stm loc) = do
        stm' <- unroll phi thetas stm
        return [C.Switch e (toBlock stm') loc]

    unroll phi thetas (C.While e stm loc) | dependsOn e phi =
        return [C.While (subst theta e) (subst theta stm) loc | theta <- thetas]

    unroll phi thetas (C.While e stm loc) = do
        stm' <- unroll phi thetas stm
        return [C.While e (toBlock stm') loc]

    unroll phi thetas (C.DoWhile stm e loc) | dependsOn e phi =
        return [C.DoWhile (subst theta stm) (subst theta e) loc | theta <- thetas]

    unroll phi thetas (C.DoWhile stm e loc) = do
        stm' <- unroll phi thetas stm
        return [C.DoWhile (toBlock stm') e loc]

    unroll phi thetas (C.For (Left inits) guard inc stm loc)
        | dependsOn inits phi || dependsOn guard phi || dependsOn inc phi =
        return [C.For (Left (subst theta inits))
                      (subst theta guard)
                      (subst theta inc)
                      (subst theta stm)
                      loc | theta <- thetas]

    unroll phi thetas (C.For (Right init) guard inc stm loc)
        | dependsOn init phi || dependsOn guard phi || dependsOn inc phi =
        return [C.For (Right (subst theta init))
                      (subst theta guard)
                      (subst theta inc)
                      (subst theta stm)
                      loc | theta <- thetas]

    unroll phi thetas (C.For inits guard inc stm loc) = do
        stms <- unroll phi thetas stm
        return [C.For inits guard inc (toBlock stms) loc]

    unroll _ _ stm@(C.Goto {}) =
        return [stm]

    unroll _ _ stm@(C.Continue {}) =
        return [stm]

    unroll _ _ stm@(C.Break {}) =
        return [stm]

    unroll _ _ stm =
        fail $ "Cannot unroll: " ++ show stm

instance Unroll (Maybe C.Stm) (Maybe C.Stm) where
    unroll _ _ Nothing =
        return Nothing

    unroll phi thetas (Just stm) = do
        stms' <- unroll phi thetas stm
        return (Just (toBlock stms'))

instance Unroll [C.Stm] [C.Stm] where
    unroll _ _ [] =
        return []

    unroll phi thetas (x : xs) = do
        x'  <- unroll phi thetas x
        xs' <- unroll phi thetas xs
        return (x' ++ xs')

class Dependencies a where
    dependencies :: a -> Set.Set String -> Set.Set String

instance Dependencies a => Dependencies (Maybe a) where
    dependencies Nothing phi  = phi
    dependencies (Just x) phi = dependencies x phi

instance Dependencies a => Dependencies [a] where
    dependencies [] phi       = phi
    dependencies (x : xs) phi = dependencies xs (dependencies x phi)

instance Dependencies C.Stm where
    dependencies (C.Label _ stm _) phi =
        dependencies stm phi

    dependencies (C.Case e stm _) phi =
        dependencies e phi `Set.union` dependencies stm phi

    dependencies (C.Default stm _) phi =
        dependencies stm phi

    dependencies (C.Exp Nothing _) phi =
        phi

    dependencies (C.Exp (Just e) _) phi =
        dependencies e phi

    dependencies (C.Block items _) phi =
        dependencies stms (dependencies inits phi)
      where
        stms = [stm | C.BlockStm stm <- items]
        inits = [init | C.BlockDecl init <- items]

    dependencies (C.If e th el _) phi =
        dependencies th phi' `Set.union` dependencies el phi'
      where
        phi' = dependencies e phi

    dependencies (C.Switch e stm _) phi =
        dependencies e phi `Set.union` dependencies stm phi

    dependencies (C.While e stm _) phi =
        dependencies e phi `Set.union` dependencies stm phi

    dependencies (C.DoWhile stm e _) phi =
        dependencies stm phi `Set.union` dependencies e phi

    dependencies (C.For (Left inits) guard inc stm _) phi =
        dependencies guard phi'
        `Set.union`
        dependencies inc phi'
        `Set.union`
        dependencies stm phi'
      where
        phi' = dependencies inits phi

    dependencies (C.For (Right init) guard inc stm _) phi =
        dependencies guard phi'
        `Set.union`
        dependencies inc phi'
        `Set.union`
        dependencies stm phi'
      where
        phi' = dependencies init phi

    dependencies (C.Goto _ _) phi = phi

    dependencies (C.Continue _) phi = phi

    dependencies (C.Break _) phi = phi

    dependencies (C.Return e _) phi = dependencies e phi

    dependencies e _ = error $ "dependencies: cannot handle " ++ show e

instance Dependencies C.Init where
    dependencies init@(C.Init (C.Id v _) _ _ _ _ _) phi
        | any (`Set.member` phi) deps = Set.insert v phi
        | otherwise                   = phi
      where
        deps :: [String]
        deps = [v | C.ExpInitializer e _ <- universeBi init,
                    C.Id v _ <- universeBi e]

    dependencies _ phi = phi

instance Dependencies C.InitGroup where
    dependencies initGroup phi =
        loop inits phi
      where
        inits :: [C.Init]
        inits = universeBi initGroup

        loop :: [C.Init] -> Phi -> Phi
        loop [] phi             = phi
        loop (init : inits) phi = loop inits (dependencies init phi)

instance Dependencies C.Exp where
    dependencies (C.Var {}) phi               = phi
    dependencies (C.Const {}) phi             = phi
    dependencies (C.BinOp _ e1 e2 _) phi      = dependencies [e1, e2] phi
    dependencies (C.Assign e1 _ e2 _) phi
        | containsOneOf e2 phi                = touchedVars e1 `Set.union` phi
        | otherwise                           = phi
    dependencies (C.PreInc e _) phi           = dependencies e phi
    dependencies (C.PostInc e _) phi          = dependencies e phi
    dependencies (C.PreDec e _) phi           = dependencies e phi
    dependencies (C.PostDec e _) phi          = dependencies e phi
    dependencies (C.UnOp _ e _) phi           = dependencies e phi
    dependencies (C.SizeofExp {}) phi         = phi
    dependencies (C.SizeofType {}) phi        = phi
    dependencies (C.Cast _ e _) phi           = dependencies e phi
    dependencies (C.Cond _ e1 e2 _) phi       = dependencies [e1, e2] phi
    dependencies (C.Member e _ _) phi         = dependencies e phi
    dependencies (C.PtrMember e _ _) phi      = dependencies e phi
    dependencies (C.Index e _ _) phi          = dependencies e phi
    dependencies (C.FnCall f es _) phi        = dependencies (f : es) phi
    dependencies (C.Seq e1 e2 _) phi          = dependencies [e1, e2] phi
    dependencies e _                          = error $ "dependencies: cannot handle " ++ show e

-- | Given an lvalue expression, return the set of variables that are
-- potentially altered when the lvalue is assigned.
touchedVars :: C.Exp -> Set.Set String
touchedVars (C.Var (C.Id id _) _) = Set.singleton id
touchedVars (C.Const {})          = Set.empty
touchedVars (C.BinOp _ e _ _)     = touchedVars e
touchedVars (C.Assign e _ _ _)    = touchedVars e
touchedVars (C.PreInc e _)        = touchedVars e
touchedVars (C.PostInc e _)       = touchedVars e
touchedVars (C.PreDec e _)        = touchedVars e
touchedVars (C.PostDec e _)       = touchedVars e
touchedVars (C.UnOp _ e _)        = touchedVars e
touchedVars (C.SizeofExp {})      = Set.empty
touchedVars (C.SizeofType {})     = Set.empty
touchedVars (C.Cast _ e _)        = touchedVars e
touchedVars (C.Cond _ e1 e2 _)    = touchedVars e1 `Set.union` touchedVars e2
touchedVars (C.Member e _ _)      = touchedVars e
touchedVars (C.PtrMember e _ _)   = touchedVars e
touchedVars (C.Index e _ _)       = touchedVars e
touchedVars (C.Seq e1 e2 _)       = touchedVars e1 `Set.union` touchedVars e2
touchedVars e                     = error $ "touchedVars: cannot handle " ++ show e
