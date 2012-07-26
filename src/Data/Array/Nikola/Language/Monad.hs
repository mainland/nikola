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

    resetH,
    shiftH,

    getFlags,

    gensym,

    lookupExp,
    insertExp,
    lookupProgH,
    insertProgH,
    lookupProgK,
    insertProgK,

    lamK,

    lamH,

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
    {  rUniq        :: Int
    ,  rFlags       :: Flags
    ,  rVarTypes    :: Map.Map Var Type
    ,  rHost        :: H ()
    ,  rExpCache    :: IntMap.IntMap [(Dynamic, Exp)]
    ,  rProgHCache  :: IntMap.IntMap [(Dynamic, ProgH)]
    ,  rProgKCache  :: IntMap.IntMap [(Dynamic, ProgK)]
    }
  deriving (Typeable)

emptyREnv :: Flags -> REnv
emptyREnv flags = REnv
    {  rUniq        = 0
    ,  rFlags       = flags
    ,  rVarTypes    = Map.empty
    ,  rHost        = R $ \s k -> k s ()
    ,  rExpCache    = IntMap.empty
    ,  rProgHCache  = IntMap.empty
    ,  rProgKCache  = IntMap.empty
    }

defaultREnv :: REnv
defaultREnv = emptyREnv defaultFlags

instance Show (StableName Exp) where
    show _ = "StableName Exp"

instance Show (StableName ProgH) where
    show _ = "StableName ProgH"

instance Show (StableName ProgK) where
    show _ = "StableName ProgK"

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

resetH :: P ProgK -> R r ProgH
resetH m = reset $ do
    (host, p) <- savingH $ reset m
    host
    return (LiftH (ProcK [] p) [])
  where
    savingH :: R r a -> R r (H (), a)
    savingH m = do
        old_host <- gets rHost
        modify $ \s -> s { rHost = R $ \s k -> k s () }
        a <- m
        host <- gets rHost
        modify $ \s -> s { rHost = old_host }
        return (host, a)

shiftH :: ((() -> H ProgH) -> H ProgH) -> P ()
shiftH m = do
    let m' = shift $ \k -> m k
    modify $ \s -> s { rHost = rHost s >> m' }

getFlags :: R r Flags
getFlags = gets rFlags

instance MonadCheck (R r) where
    lookupVarType v = do
        maybe_tau <- gets $ \s -> Map.lookup v (rVarTypes s)
        case maybe_tau of
          Just tau -> return tau
          Nothing ->  faildoc $ text "Variable" <+> ppr v <+>
                                text "not in scope during reification."

    extendVarTypes vtaus act = do
        old_vars <- gets rVarTypes
        modify $ \s -> s { rVarTypes = foldl' insert (rVarTypes s) vtaus }
        x  <- act
        modify $ \s -> s { rVarTypes = old_vars }
        return x
      where
        insert m (k, v) = Map.insert k v m

-- | Our monad for constructing host programs
type H a = R ProgH a

-- | Our monad for constructing kernels
type P a = R ProgK a

gensym :: String -> R r Var
gensym v = do
    u <- gets rUniq
    modify $ \s -> s { rUniq = u + 1 }
    return $ Var (v ++ "_" ++ show u)

lookupExp :: Typeable a => StableName a -> R r (Maybe Exp)
lookupExp sn =
    lookupCache sn rExpCache

insertExp :: Typeable a => StableName a -> Exp -> R r ()
insertExp sn e =
    modify $ \s -> s { rExpCache = insertCache sn e (rExpCache s) }

lookupProgH :: Typeable a => StableName a -> R r (Maybe ProgH)
lookupProgH sn =
    lookupCache sn rProgHCache

insertProgH :: Typeable a => StableName a -> ProgH -> R r ()
insertProgH sn p =
    modify $ \s -> s { rProgHCache = insertCache sn p (rProgHCache s) }

lookupProgK :: Typeable a => StableName a -> R r (Maybe ProgK)
lookupProgK sn =
    lookupCache sn rProgKCache

insertProgK :: Typeable a => StableName a -> ProgK -> R r ()
insertProgK sn p =
    modify $ \s -> s { rProgKCache = insertCache sn p (rProgKCache s) }

lookupCache :: Typeable a
            => StableName a
            -> (REnv -> IntMap.IntMap [(Dynamic, b)])
            -> R r (Maybe b)
lookupCache sn lbl = do
    cache <- gets lbl
    case IntMap.lookup hash cache of
      Just xs -> return $ Prelude.lookup  (Just sn)
                                          [(fromDynamic d,x) | (d,x) <- xs]
      Nothing -> return Nothing
  where
    hash :: StableNameHash
    hash = hashStableName sn

insertCache :: forall a b . Typeable a
            => StableName a
            -> b
            -> IntMap.IntMap [(Dynamic, b)]
            -> IntMap.IntMap [(Dynamic, b)]
insertCache sn x cache =
    IntMap.alter add (hashStableName sn) cache
  where
    add :: Maybe [(Dynamic, b)] -> Maybe [(Dynamic, b)]
    add Nothing   = Just [(toDyn sn, x)]
    add (Just xs) = Just ((toDyn sn, x) : xs)

lamK :: [(Var, Type)] -> R ProcK ProcK -> R ProcK ProcK
lamK vtaus m = do
    ProcK vtaus' p <- reset $ extendVarTypes vtaus m
    return $ ProcK (vtaus ++ vtaus') p

lamH :: [(Var, Type)] -> R ProcH ProcH -> R ProcH ProcH
lamH vtaus m = do
    ProcH vtaus' p <- reset $ extendVarTypes vtaus m
    return $ ProcH (vtaus ++ vtaus') p

lamE :: [(Var, Type)] -> R Exp Exp -> R Exp Exp
lamE vtaus m = do
    e <- reset $ extendVarTypes vtaus m
    case e of
      LamE vtaus' e  -> return $ LamE (vtaus ++ vtaus') e
      _              -> return $ LamE vtaus e

letE :: Var -> Type -> Exp -> R Exp Exp
letE v tau e =
    shift $ \k -> do
    body <- reset $ extendVarTypes [(v, tau)] $ do
            k (VarE v)
    return $ LetE v tau Many e body

mkLetE :: Exp -> R Exp Exp
mkLetE e@(VarE {})   = return e
mkLetE e@(ConstE {}) = return e
mkLetE e@(UnitE {})  = return e
mkLetE e             = do  v    <- gensym "v"
                           tau  <- inferExp e
                           letE v tau e

inNewScope :: Bool -> R a a -> R r a
inNewScope isLambda comp = do
    a <- reset $ do
         expCache   <- gets rExpCache
         hprogCache <- gets rProgHCache
         kprogCache <- gets rProgKCache
         when isLambda $ do
           modify $ \s -> s { rExpCache   = IntMap.empty
                            , rProgHCache = IntMap.empty
                            , rProgKCache = IntMap.empty
                            }
         a <- comp
         modify $ \s -> s { rExpCache   = expCache
                          , rProgHCache = hprogCache
                          , rProgKCache = kprogCache
                          }
         return a
    return a
