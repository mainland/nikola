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

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Nikola.Compile (
    withCompiledCFun,
    withCompiledFunction,
    withCompiledFunctionEx,

    compileIO,
    compile,
    compileTH,
    compileTHEx
  ) where

import Prelude hiding (map, zipWith)

import Control.Applicative
import Control.Exception
import Control.Monad.Trans (MonadIO(..))
import qualified Data.ByteString.Char8 as B
import qualified Foreign.CUDA.Driver as CU
import Language.Haskell.TH (Q,
                            ExpQ,
                            runIO,
                            stringE)
import Language.Haskell.TH.Syntax (Lift(..))
import System.IO.Unsafe (unsafePerformIO)

import Nikola.Backend.CUDA.CodeGen
import Nikola.Exec
import qualified Nikola.Nvcc as Nvcc
import Nikola.Quote
import Nikola.Reify
import Nikola.Representable
import Nikola.Syntax (Exp, Tau, ExecConfig)

withModuleFromByteString :: B.ByteString -> (CU.Module -> IO a) -> IO a
withModuleFromByteString bs =
    bracket (CU.loadData bs) CU.unload

withCompiledCFunEx  :: CFun a
                    -> (ExState -> IO b)
                    -> IO b
withCompiledCFunEx cfun kont = do
    bs <- Nvcc.compile (cfunDefs cfun)
    withModuleFromByteString bs $ \mod -> do
    cudaFun <- CU.getFun mod (cfunName cfun)
    kont emptyExState { exFun    = cudaFun
                      , exLaunch = cfunLaunch cfun
                      }

withCompiledCFun :: CFun a
                 -> (F a -> IO b)
                 -> IO b
withCompiledCFun cfun act =
    withCompiledCFunEx cfun (act . F)

withCompiledFunctionEx :: (ReifiableFun a b)
                       => ROpts
                       -> (a -> b)
                       -> (F (a -> b) -> IO c)
                       -> IO c
withCompiledFunctionEx ropts f act = do
    cfun <- reifyEx ropts f >>= compileTopFun fname
    withCompiledCFunEx cfun (act . F)
  where
    fname = "f"

withCompiledFunction :: (ReifiableFun a b)
                     => (a -> b)
                     -> (F (a -> b) -> IO c)
                     -> IO c
withCompiledFunction =
    withCompiledFunctionEx defaultROpts

cfunLaunch :: CFun a -> Ex b -> Ex b
cfunLaunch cfun comp = do
    mapM_ alloc (cfunAllocs cfun)
    (dimX, dimY, dimZ, gridW, gridH) <- configToLaunchParams (cfunExecConfig cfun)
    defaultLaunch dimX dimY dimZ gridW gridH comp

-- | Compile an 'a' into a function of type 'IOFun a' that returns its result in
-- the 'IO' monad.
class CompileIO a where
    type IOFun a :: *

    compileIO :: a -> IOFun a

    compilekIO :: IO (F a) -> (forall a . Ex a -> Ex a) -> IOFun a

instance (Representable a,
          Representable b) => CompileIO (Exp a -> Exp b) where
    type IOFun (Exp a -> Exp b) = a -> IO b

    compileIO f = compilekIO sigma id
      where
        sigma :: IO (F (Exp a -> Exp b))
        sigma = do
          F <$> reifyCompileAndLoad defaultROpts f

    compilekIO sigma args = \x -> do
        sigma' <- sigma
        flip evalEx (unF sigma') $
            args $
            withArg x $
            launchKernel $
            returnResult

instance (Representable a,
          Representable b) => CompileIO (Exp a -> IO (Exp b)) where
    type IOFun (Exp a -> IO (Exp b)) = a -> IO b

    compileIO f = compilekIO sigma id
      where
        sigma :: IO (F (Exp a -> Exp b))
        sigma = do
          F <$> reifyCompileAndLoad defaultROpts f

    compilekIO sigma args = \x -> do
        sigma' <- sigma
        flip evalEx (unF sigma') $
            args $
            withArg x $
            launchKernel $
            returnResult

instance (Representable a,
          ReifiableFun (Exp a) (b -> c),
          CompileIO (b -> c)) => CompileIO (Exp a -> b -> c) where
    type IOFun (Exp a -> b -> c) = a -> IOFun (b -> c)

    compileIO f = compilekIO sigma id
      where
        sigma :: IO (F (Exp a -> b -> c))
        sigma = F <$> reifyCompileAndLoad defaultROpts f

    compilekIO sigma args = \x -> compilekIO sigma' (args . withArg x)
      where
        sigma' ::  IO (F (b -> c))
        sigma' = castF <$> sigma

instance (CompileIO (a -> b)) => CompileIO (CFun (a -> b)) where
    type IOFun (CFun (a -> b)) = IOFun (a -> b)

    compileIO cfun = compilekIO f id
      where
        f :: IO (F (a -> b))
        f = F <$> compileAndLoad cfun

    compilekIO = compilekIO

instance (CompileIO (a -> b)) => CompileIO (F (a -> b)) where
    type IOFun (F (a -> b)) = IOFun (a -> b)

    compileIO f = compilekIO (return f) id

    compilekIO = compilekIO

-- | Compile an 'a' into a pure function of type 'Fun a'.
class Compile a where
    type Fun a :: *

    compile  :: a -> Fun a

    -- Note that |compilek| has a different type here than in the |CompileIO|
    -- class---this is very important! It means that the function is reified and
    -- compiled /once/ and then reused on future invocations. In the |compilek|
    -- in |CompileIO|, the function is reified and compiled /after/ all
    -- arguments have been supplied meaning it is compiled every time the
    -- function is evaluated.
    compilek :: F a
             -> (forall a . Ex a -> Ex a)
             -> Fun a

instance (Representable a, Representable b) => Compile (Exp a -> Exp b) where
    type Fun (Exp a -> Exp b) = a -> b

    compile f = compilek sigma id
      where
        sigma :: F (Exp a -> Exp b)
        sigma = F $ unsafePerformIO $ reifyCompileAndLoad defaultROpts f

    compilek sigma args = \x -> unsafePerformIO $
        flip evalEx (unF sigma) $
            args $
            withArg x $
            launchKernel $
            returnResult

instance (Representable a, Representable b) => Compile (Exp a -> IO (Exp b)) where
    type Fun (Exp a -> IO (Exp b)) = a -> IO b

    compile f = compilek sigma id
      where
        sigma :: F (Exp a -> IO (Exp b))
        sigma = F $ unsafePerformIO $ reifyCompileAndLoad defaultROpts f

    compilek sigma args = \x ->
        flip evalEx (unF sigma) $
            args $
            withArg x $
            launchKernel $
            returnResult

instance (Representable a,
          ReifiableFun (Exp a) (b -> c),
          Compile (b -> c)) => Compile (Exp a -> b -> c) where
    type Fun (Exp a -> b -> c) = a -> Fun (b -> c)

    compile f = compilek sigma id
      where
        sigma :: F (Exp a -> b -> c)
        sigma = F $ unsafePerformIO $ reifyCompileAndLoad defaultROpts f

    compilek sigma args = \x -> compilek sigma' (args . withArg x)
      where
        sigma' :: F (b -> c)
        sigma' = castF sigma

instance (Compile (a -> b)) => Compile (CFun (a -> b)) where
    type Fun (CFun (a -> b)) = Fun (a -> b)

    compile cfun = compilek f id
      where
        f :: F (a -> b)
        f = F $ unsafePerformIO $ compileAndLoad cfun

    compilek = compilek

instance (Compile (a -> b)) => Compile (F (a -> b)) where
    type Fun (F (a -> b)) = Fun (a -> b)

    compile f = compilek f id

    compilek = compilek

reifyCompileAndLoad :: ReifiableFun a b
                    => ROpts
                    -> (a -> b)
                    -> IO ExState
reifyCompileAndLoad ropts f =
    reifyEx ropts f >>= compileTopFun fname >>= compileAndLoad
  where
    fname = "f"

compileAndLoad :: CFun a -> IO ExState
compileAndLoad cfun = do
    mod     <- Nvcc.compile (cfunDefs cfun) >>= CU.loadData
    cudaFun <- CU.getFun mod (cfunName cfun)
    return emptyExState { exFun    = cudaFun
                        , exLaunch = cfunLaunch cfun
                        }

-- | Compile an 'a' into a Template Haskell expression of type 'LiftFun a'.
class CompileLift a where
    type LiftFun a :: *

    compileLift :: ROpts -> a -> QExp (LiftFun a)

    compilekLift :: QExp (F a)
                 -> QExp (forall a . Ex a -> Ex a)
                 -> QExp (LiftFun a)

instance (Representable a,
          Representable b) => CompileLift (Exp a -> Exp b) where
    type LiftFun (Exp a -> Exp b) = a -> b

    compileLift ropts f = compilekLift sigma (QExp [|id|])
      where
        sigma :: QExp (F (Exp a -> Exp b))
        sigma = QExp [|F $ unsafePerformIO $(reifyCompileAndLoadTH ropts f)|]

    compilekLift sigma args = QExp [|\x ->
        unsafePerformIO $
        flip evalEx (unF $(unQExp sigma)) $
        $(unQExp args) $
        withArg x $
        launchKernel $
        returnResult|]

instance (Representable a, Representable b) => CompileLift (Exp a -> IO (Exp b)) where
    type LiftFun (Exp a -> IO (Exp b)) = a -> IO b

    compileLift ropts f = compilekLift sigma (QExp [|id|])
      where
        sigma :: QExp (F (Exp a -> IO (Exp b)))
        sigma = QExp [|F $ unsafePerformIO $(reifyCompileAndLoadTH ropts f)|]

    compilekLift sigma args = QExp [|\x ->
        flip evalEx (unF $(unQExp sigma)) $
        $(unQExp args) $
        withArg x $
        launchKernel $
        returnResult|]

instance (CompileLift (a -> b)) => CompileLift (CFun (a -> b)) where
    type LiftFun (CFun (a -> b)) = LiftFun (a -> b)

    compileLift ropts cfun = compilekLift f (QExp [|id|])
      where
        f :: QExp (F (a -> b))
        f = QExp [|F $ unsafePerformIO $(compileAndLoadTH cfun)|]

    compilekLift = compilekLift

instance (Representable a,
          ReifiableFun (Exp a) (b -> c),
          CompileLift (b -> c)) => CompileLift (Exp a -> b -> c) where
    type LiftFun (Exp a -> b -> c) = a -> LiftFun (b -> c)

    compileLift ropts f = compilekLift sigma (QExp [|id|])
      where
        sigma :: QExp (F (Exp a -> b -> c))
        sigma = QExp [|F $ unsafePerformIO $(reifyCompileAndLoadTH ropts f)|]

    compilekLift sigma args =
        QExp [|\x -> $(unQExp (compilekLift sigma' (QExp [|$(unQExp args) . withArg x|])))|]
      where
        sigma' ::  QExp (F (b -> c))
        sigma' = QExp [|castF $(unQExp sigma)|]

newtype QExp a = QExp { unQExp :: ExpQ }

reifyCompileAndLoadTH :: ReifiableFun a b
                      => ROpts
                      -> (a -> b)
                      -> ExpQ
reifyCompileAndLoadTH ropts f = do
    cfun <- liftIO $ reifyEx ropts f >>= compileTopFun fname
    compileAndLoadTH cfun
  where
    fname :: String
    fname = "f"

compileAndLoadTH :: CFun a
                 -> ExpQ
compileAndLoadTH cfun = do
    bs <- liftIO $ Nvcc.compile (cfunDefs cfun)
    [|do { mod     <- CU.loadData (B.pack $(stringE (B.unpack bs)))
         ; cudaFun <- CU.getFun mod $(lift (cfunName cfun))
         ; let sigma = emptyExState { exFun    = cudaFun
                                    , exLaunch = $(cfunLaunchTH cfun)
                                    }
         ; return sigma
         }|]
  where
    cfunLaunchTH :: CFun a -> ExpQ
    cfunLaunchTH cfun =
        [|\comp ->
             do { mapM_ alloc $qAllocs
                ; (dimX, dimY, dimZ, gridW, gridH) <- configToLaunchParams $qExecConfig
                ; defaultLaunch dimX dimY dimZ gridW gridH comp
                }|]

      where
        qAllocs :: ExpQ
        qAllocs = lift (cfunAllocs cfun)

        qExecConfig :: ExpQ
        qExecConfig = lift (cfunExecConfig cfun)

instance Lift Tau where
    lift = dataToExpQ (const Nothing)

instance Lift ExecConfig where
    lift = dataToExpQ (const Nothing)

instance MonadIO Q where
    liftIO = runIO

compileTHEx :: CompileLift a => ROpts -> a -> ExpQ
compileTHEx ropts = unQExp . compileLift ropts

compileTH :: CompileLift a => a -> ExpQ
compileTH = compileTHEx defaultROpts
