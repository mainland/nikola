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

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Nikola.Compile (
    withCompiledCFun,
    withCompiledFunction',
    withCompiledFunction,

    call,
    compile,
    compileTH,
    compileTH'
  ) where

import Prelude hiding (map, zipWith)

import Control.Applicative
import Control.Exception
import Control.Monad.Trans (MonadIO(..))
import qualified Data.ByteString.Char8 as B
import Data.Data
import qualified Foreign.CUDA.Driver as CU
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH hiding (Exp, reify)
import Language.Haskell.TH.Syntax hiding (Exp, reify)
import System.IO.Unsafe (unsafePerformIO)

import Nikola.Embeddable
import Nikola.Exec
import Nikola.Nvcc
import Nikola.Reify
import Nikola.Syntax (Exp)
import Nikola.ToC

withModuleFromByteString :: B.ByteString -> (CU.Module -> IO a) -> IO a
withModuleFromByteString bs =
    bracket (CU.loadData bs) CU.unload

withRawCompiledCFun  :: CFun a
                     -> (ExState -> IO b)
                     -> IO b
withRawCompiledCFun cfun act = do
    bs <- compileFunction (cfunDefs cfun)
    withModuleFromByteString bs $ \mod -> do
    cudaFun <- CU.getFun mod (cfunName cfun)
    act emptyExState { exFun    = cudaFun
                     , exLaunch = cfunLaunch cfun
                     }

withCompiledCFun :: CFun a
                 -> (F a -> IO b)
                 -> IO b
withCompiledCFun cfun act =
    withRawCompiledCFun cfun (act . F)

withCompiledFunction' :: (ReifiableFun a b)
                      => ROpts
                      -> (a -> b)
                      -> (F (a -> b) -> IO c)
                      -> IO c
withCompiledFunction' ropts f act = do
    cfun <- reify' ropts f >>= compileTopFun fname
    withRawCompiledCFun cfun (act . F)
  where
    fname = "f"

withCompiledFunction :: (ReifiableFun a b)
                     => (a -> b)
                     -> (F (a -> b) -> IO c)
                     -> IO c
withCompiledFunction =
    withCompiledFunction' defaultROpts

cfunLaunch :: CFun a -> Ex b -> Ex b
cfunLaunch cfun comp = do
    mapM_ alloc (cfunAllocs cfun)
    (dimX, dimY, dimZ, gridW, gridH) <- configToLaunchParams (cfunExecConfig cfun)
    defaultLaunch dimX dimY dimZ gridW gridH comp

class Callable a b | a -> b where
    -- | Transform an 'a' into a callable function of type 'b'.
    call :: a -> b

instance CompilableIO a b c => Callable (a -> b) c where
    call = compileIO

instance CompilableIO a b c => Callable (CFun (a -> b)) c where
    call cfun = compilekIO f id
      where
        f :: IO (F (a -> b))
        f = F <$> snd <$> compileAndLoad cfun

instance CompilableIO a b c => Callable (F (a -> b)) c where
    call f = compilekIO (return f) id

class CompilableIO a b c | a b -> c where
    compileIO  :: (a -> b) -> c

    compilekIO :: IO (F (a -> b)) -> (forall a . Ex a -> Ex a) -> c

instance (Embeddable a, Embeddable b) =>
  CompilableIO (Exp a) (Exp b) (a -> IO b) where
    compileIO f = compilekIO sigma id
      where
        sigma :: IO (F (Exp a -> Exp b))
        sigma = do
          F <$> snd <$> reifyCompileAndLoad defaultROpts f

    compilekIO sigma args = \x -> do
        sigma' <- sigma
        flip evalEx (unF sigma') $
            args $
            withArg x $
            launchKernel $
            returnResult

instance (Embeddable a, Embeddable b) =>
  CompilableIO (Exp a) (IO (Exp b)) (a -> IO b) where
    compileIO f = compilekIO sigma id
      where
        sigma :: IO (F (Exp a -> Exp b))
        sigma = do
          F <$> snd <$> reifyCompileAndLoad defaultROpts f

    compilekIO sigma args = \x -> do
        sigma' <- sigma
        flip evalEx (unF sigma') $
            args $
            withArg x $
            launchKernel $
            returnResult

instance (Embeddable a, ReifiableFun (Exp a) (b -> c), CompilableIO b c d) =>
  CompilableIO (Exp a) (b -> c) (a -> d) where
    compileIO f = compilekIO sigma id
      where
        sigma :: IO (F (Exp a -> b -> c))
        sigma = F <$> snd <$> reifyCompileAndLoad defaultROpts f

    compilekIO sigma args = \x -> compilekIO sigma' (args . withArg x)
      where
        sigma' ::  IO (F (b -> c))
        sigma' = castF <$> sigma

class Compilable a b | a -> b where
    -- | Transform an 'a' into a callable function of type 'b'.
    compile :: a -> b

instance CompilablePure a b c => Compilable (a -> b) c where
    compile = compilePure

instance CompilablePure a b c => Compilable (CFun (a -> b)) c where
    compile cfun = compilekPure f id
      where
        f :: F (a -> b)
        f = F $ snd $ unsafePerformIO (compileAndLoad cfun)

instance CompilablePure a b c => Compilable (F (a -> b)) c where
    compile f = compilekPure f id

class CompilablePure a b c | a b -> c where
    compilePure :: (a -> b) -> c

    compilekPure :: F (a -> b) -> (forall a . Ex a -> Ex a) -> c

instance (Embeddable a, Embeddable b) =>
  CompilablePure (Exp a) (Exp b) (a -> b) where
    compilePure f = compilekPure sigma id
      where
        sigma :: F (Exp a -> Exp b)
        sigma = F $ snd $ unsafePerformIO (reifyCompileAndLoad defaultROpts f)

    compilekPure sigma args = \x ->
        unsafePerformIO $
        flip evalEx (unF sigma) $
        args $
        withArg x $
        launchKernel $
        returnResult

instance (Embeddable a, Embeddable b) =>
  CompilablePure (Exp a) (IO (Exp b)) (a -> IO b) where
    compilePure f = compilekPure sigma id
      where
        sigma :: F (Exp a -> IO (Exp b))
        sigma = F $ snd $ unsafePerformIO (reifyCompileAndLoad defaultROpts f)

    compilekPure sigma args = \x ->
        flip evalEx (unF sigma) $
        args $
        withArg x $
        launchKernel $
        returnResult

instance (Embeddable a, ReifiableFun (Exp a) (b -> c), CompilablePure b c d) =>
  CompilablePure (Exp a) (b -> c) (a -> d) where
    compilePure f = compilekPure sigma id
      where
        sigma :: F (Exp a -> b -> c)
        sigma = F $ snd $ unsafePerformIO (reifyCompileAndLoad defaultROpts f)

    compilekPure sigma args = \x -> compilekPure sigma' (args . withArg x)
      where
        sigma' ::  F (b -> c)
        sigma' = castF sigma

reifyCompileAndLoad :: ReifiableFun a b
                    => ROpts
                    -> (a -> b)
                    -> IO (CU.Module, ExState)
reifyCompileAndLoad ropts f =
    reify' ropts f >>= compileTopFun fname >>= compileAndLoad
  where
    fname = "f"

compileAndLoad :: CFun a
               -> IO (CU.Module, ExState)
compileAndLoad cfun = do
    mod     <- compileFunction (cfunDefs cfun) >>= CU.loadData
    cudaFun <- CU.getFun mod (cfunName cfun)
    let sigma = emptyExState { exFun    = cudaFun
                             , exLaunch = cfunLaunch cfun
                             }
    return (mod, sigma)

newtype QExp a = QExp { unQExp :: ExpQ }

class CompilablePureTH a b c | a b -> c where
    compilePureTH :: ROpts -> (a -> b) -> QExp c

    compilekPureTH :: QExp (F (a -> b))
                   -> QExp (forall a . Ex a -> Ex a)
                   -> QExp c

instance (Embeddable a, Embeddable b) =>
  CompilablePureTH (Exp a) (Exp b) (a -> b) where
    compilePureTH ropts f = compilekPureTH sigma (QExp [|id|])
      where
        sigma :: QExp (F (Exp a -> Exp b))
        sigma = QExp [|F $ snd $ unsafePerformIO $(reifyCompileAndLoadTH ropts f)|]

    compilekPureTH sigma args = QExp [|\x ->
        unsafePerformIO $
        flip evalEx (unF $(unQExp sigma)) $
        $(unQExp args) $ do
        withArg x $ do
        launchKernel $ do
        returnResult|]

instance (Embeddable a, Embeddable b) =>
  CompilablePureTH (Exp a) (IO (Exp b)) (a -> IO b) where
    compilePureTH ropts f = compilekPureTH sigma (QExp [|id|])
      where
        sigma :: QExp (F (Exp a -> IO (Exp b)))
        sigma = QExp [|F $ snd $ unsafePerformIO $(reifyCompileAndLoadTH ropts f)|]

    compilekPureTH sigma args = QExp [|\x ->
        flip evalEx (unF $(unQExp sigma)) $
        $(unQExp args) $ do
        withArg x $ do
        launchKernel $ do
        returnResult|]

instance (Embeddable a, ReifiableFun (Exp a) (b -> c), CompilablePureTH b c d) =>
  CompilablePureTH (Exp a) (b -> c) (a -> d) where
    compilePureTH ropts f = compilekPureTH sigma (QExp [|id|])
      where
        sigma :: QExp (F (Exp a -> b -> c))
        sigma = QExp [|F $ snd $ unsafePerformIO $(reifyCompileAndLoadTH ropts f)|]

    compilekPureTH sigma args =
        QExp [|\x -> $(unQExp (compilekPureTH sigma' (QExp [|$(unQExp args) . withArg x|])))|]
      where
        sigma' ::  QExp (F (b -> c))
        sigma' = QExp [|castF $(unQExp sigma)|]

compileTH :: CompilableTH a => a -> ExpQ
compileTH = compileTH' defaultROpts

class CompilableTH a where
    compileTH' :: ROpts -> a -> ExpQ

instance CompilablePureTH a b c => CompilableTH (a -> b) where
    compileTH' ropts = unQExp . compilePureTH ropts

instance CompilablePureTH a b c => CompilableTH (CFun (a -> b)) where
    compileTH' _ cfun = unQExp $ compilekPureTH f (QExp [|id|])
      where
        f :: QExp (F (a -> b))
        f = QExp [|F $ snd $ unsafePerformIO $(compileAndLoadTH cfun)|]

reifyCompileAndLoadTH :: ReifiableFun a b
                      => ROpts
                      -> (a -> b)
                      -> ExpQ
reifyCompileAndLoadTH ropts f = do
    cfun <- liftIO $ reify' ropts f >>= compileTopFun fname
    compileAndLoadTH cfun
  where
    fname :: String
    fname = "f"

compileAndLoadTH :: CFun a
                 -> ExpQ
compileAndLoadTH cfun = do
    bs <- liftIO $ compileFunction (cfunDefs cfun)
    [|do { mod     <- CU.loadData (B.pack $(stringE (B.unpack bs)))
         ; cudaFun <- CU.getFun mod $(lift (cfunName cfun))
         ; let sigma = emptyExState { exFun    = cudaFun
                                    , exLaunch = $(cfunLaunchTH cfun)
                                    }
         ; return (mod, sigma)
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

instance Data a => Lift a where
    lift = dataToExpQ (const Nothing)

instance MonadIO Q where
    liftIO = runIO

dataToQa  ::  forall a k q. Data a
          =>  (Name -> k)
          ->  (Lit -> Q q)
          ->  (k -> [Q q] -> Q q)
          ->  (forall b . Data b => b -> Maybe (Q q))
          ->  a
          ->  Q q
dataToQa mkCon mkLit appCon antiQ t =
    case antiQ t of
      Nothing ->
          case constrRep constr of
            AlgConstr _  ->
                appCon con conArgs
            IntConstr n ->
                mkLit $ integerL n
            FloatConstr n ->
                mkLit $ rationalL (toRational n)
            CharConstr c ->
                mkLit $ charL c
        where
          constr :: Constr
          constr = toConstr t

          con :: k
          con = mkCon (mkName' mod occ)
            where
              mod :: String
              mod = (tyconModule . dataTypeName . dataTypeOf) t

              occ :: String
              occ = showConstr constr

              mkName' :: String -> String -> Name
              mkName' "Prelude" "(:)" = Name (mkOccName ":") NameS
              mkName' "Prelude" "[]"  = Name (mkOccName "[]") NameS
              mkName' "Prelude" "()"  = Name (mkOccName "()") NameS

              mkName' "Prelude" s@('(' : ',' : rest) = go rest
                where
                  go :: String -> Name
                  go (',' : rest) = go rest
                  go ")"          = Name (mkOccName s) NameS
                  go _            = Name (mkOccName occ) (NameQ (mkModName mod))

              mkName' "GHC.Real" ":%" = mkNameG_d "base" "GHC.Real" ":%"

              mkName' mod occ = Name (mkOccName occ) (NameQ (mkModName mod))

          conArgs :: [Q q]
          conArgs = gmapQ (dataToQa mkCon mkLit appCon antiQ) t

      Just y -> y

-- | 'dataToExpQ' converts a value to a 'Q Exp' representation of the same
-- value. It takes a function to handle type-specific cases.
dataToExpQ  ::  Data a
            =>  (forall b . Data b => b -> Maybe TH.ExpQ)
            ->  a
            ->  ExpQ
dataToExpQ = dataToQa conE litE (foldl appE)

-- | 'dataToPatQ' converts a value to a 'Q Pat' representation of the same
-- value. It takes a function to handle type-specific cases.
dataToPatQ  ::  Data a
            =>  (forall b . Data b => b -> Maybe TH.PatQ)
            ->  a
            ->  PatQ
dataToPatQ = dataToQa id litP conP
