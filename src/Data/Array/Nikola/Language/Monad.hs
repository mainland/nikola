{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Data.Array.Nikola.Language.Monad
-- Copyright   : (c) Geoffrey Mainland 2012
-- License     : BSD-style
--
-- Maintainer  : Geoffrey Mainland <mainland@apeiron.net>
-- Stability   : experimental
-- Portability : non-portable

module Data.Array.Nikola.Language.Monad (
    emptyREnv,
    defaultREnv,
    REnv,

    R,
    H,
    P,
    runR,
    reset,
    shift,

    getFlags,

    gensym,

    lookupExp,
    extendExps,

    lookupStableName,
    insertStableName,

    lamE,
    letE,
    mkLetE,

    inNewScope
  ) where

import Control.Applicative
import Control.Monad.Cont
import Control.Monad.State
import Data.Dynamic
import qualified Data.IntMap as IntMap
import Data.List (foldl')
import qualified Data.Map as Map
import System.Mem.StableName
import Text.PrettyPrint.Mainland

import Data.Array.Nikola.Backend.Flags
import Data.Array.Nikola.Language.Check
import Data.Array.Nikola.Language.Syntax
-- import Data.Array.Nikola.Pretty

type StableNameHash = Int

data REnv = REnv
    {  rUniq         :: Int
    ,  rFlags        :: Flags
    ,  rContext      :: Context
    ,  rVarTypes     :: Map.Map Var Type
    ,  rExpCache     :: Map.Map Exp Exp
    ,  rSharingCache :: IntMap.IntMap [(Dynamic, Exp)]
    }
  deriving (Typeable)

emptyREnv :: Flags -> REnv
emptyREnv flags = REnv
    {  rUniq         = 0
    ,  rFlags        = flags
    ,  rContext      = Host
    ,  rVarTypes     = Map.empty
    ,  rExpCache     = Map.empty
    ,  rSharingCache = IntMap.empty
    }

defaultREnv :: REnv
defaultREnv = emptyREnv defaultFlags

instance Show (StableName Exp) where
    show _ = "StableName Exp"

newtype R r a = R { unR :: REnv -> (REnv -> a -> IO (REnv, r)) -> IO (REnv, r) }
  deriving (Typeable)

runR :: R a a -> REnv -> IO (REnv, a)
runR m env = unR m env (\s x -> return (s, x))

instance Monad (R r) where
    return a = R $ \s k -> k s a

    m >>= f  = R $ \s k -> unR m  s $ \s' x -> unR (f x) s' k

    m1 >> m2 = R $ \s k -> unR m1 s $ \s' _ -> unR m2    s' k

    fail err = R $ \_ _ -> fail err

instance Functor (R r) where
    fmap f x = x >>= return . f

instance Applicative (R r) where
    pure   = return
    (<*>)  = ap

instance MonadState REnv (R r) where
    get   = R $ \s k -> k s s
    put s = R $ \_ k -> k s ()

instance MonadIO (R r) where
    liftIO m = R $ \s k -> m >>= \x -> k s x

instance MonadCont (R r) where
    -- callCC :: ((a -> R r b) -> R r a) -> R r a
    callCC f = R $ \s k -> unR (f (\a -> R $ \s _ -> k s a)) s k

reset :: R a a -> R r a
reset m = R $ \s k -> do  (s', x) <- unR m s $ \s' x -> return (s', x)
                          k s' x

shift :: ((a -> R r r) -> R r r) -> R r a
shift f = R $ \s k ->
    let c = \x -> R $ \s k' -> do  (s', y) <- k s x
                                   k' s' y
    in
      runR (f c) s

getFlags :: R r Flags
getFlags = gets rFlags

instance MonadCheck (R r) where
    getContext = gets rContext

    setContext ctx = modify $ \s -> s { rContext = ctx }

    lookupVarType v = do
        maybe_tau <- gets $ \s -> Map.lookup v (rVarTypes s)
        case maybe_tau of
          Just tau -> return tau
          Nothing ->  faildoc $ text "Variable" <+> ppr v <+>
                                text "not in scope."

    extendVarTypes vtaus act = do
        old_vtaus <- gets rVarTypes
        modify $ \s -> s { rVarTypes = foldl' insert (rVarTypes s) vtaus }
        x  <- act
        modify $ \s -> s { rVarTypes = old_vtaus }
        return x
      where
        insert m (k, v) = Map.insert k v m

-- | Our monad for constructing host programs
type H a = R Exp a

-- | Our monad for constructing kernels
type P a = R Exp a

gensym :: String -> R r Var
gensym v = do
    u <- gets rUniq
    modify $ \s -> s { rUniq = u + 1 }
    return $ Var (v ++ "_" ++ show u)

lookupExp :: Exp -> R r (Maybe Exp)
lookupExp e =
    gets $ \s -> Map.lookup e (rExpCache s)

extendExps :: [(Exp, Exp)] -> R r a -> R r a
extendExps es kont = do
    cache <- gets rExpCache
    modify $ \s -> s { rExpCache = foldl' insert (rExpCache s) es }
    x <- kont
    modify $ \s -> s { rExpCache = cache }
    return x
  where
    insert m (k, v) = Map.insert k v m

lookupStableName :: Typeable a => StableName a -> R r (Maybe Exp)
lookupStableName sn = do
    cache <- gets rSharingCache
    case IntMap.lookup hash cache of
      Just xs -> return $ Prelude.lookup  (Just sn)
                                          [(fromDynamic d,x) | (d,x) <- xs]
      Nothing -> return Nothing
  where
    hash :: StableNameHash
    hash = hashStableName sn

insertStableName :: Typeable a => StableName a -> Exp -> R r ()
insertStableName sn e =
    modify $ \s ->
        s { rSharingCache = IntMap.alter add (hashStableName sn)
                                             (rSharingCache s) }
  where
    add :: Maybe [(Dynamic, Exp)] -> Maybe [(Dynamic, Exp)]
    add Nothing   = Just [(toDyn sn, e)]
    add (Just xs) = Just ((toDyn sn, e) : xs)

lamE :: [(Var, Type)] -> R Exp Exp -> R Exp Exp
lamE vtaus m = do
    e <- extendVarTypes vtaus $ reset $ m
    case e of
      LamE vtaus' e  -> return $ LamE (vtaus ++ vtaus') e
      _              -> return $ LamE vtaus e

letE :: Var -> Type -> Exp -> R Exp Exp
letE v tau e =
    shift $ \k -> do
    body <- extendVarTypes [(v, tau)] $
            extendExps [(e, VarE v)] $
            reset $
            k (VarE v)
    return $ LetE v tau Many e body

-- We never let-bind atomic expressions or monadic values.
mkLetE :: Exp -> R Exp Exp
mkLetE e | isAtomicE e =
    return e

mkLetE e = do
    tau <- inferExp e
    if isMT tau
      then return e
      else do v <- if isFunT tau then gensym "f" else gensym "v"
              letE v tau e

inNewScope :: Bool -> R a a -> R r a
inNewScope isLambda comp = do
    expCache     <- gets rExpCache
    sharingCache <- gets rSharingCache
    when isLambda $ do
      modify $ \s -> s { rExpCache     = Map.empty
                       , rSharingCache = IntMap.empty
                       }
    a <- reset comp
    modify $ \s -> s { rExpCache     = expCache
                     , rSharingCache = sharingCache
                     }
    return a
