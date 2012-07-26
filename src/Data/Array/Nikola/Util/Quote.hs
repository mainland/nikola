{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Data.Array.Nikola.Util.Quote
-- Copyright   : (c) The President and Fellows of Harvard College 2006-2010
-- Copyright   : (c) Geoffrey Mainland 2012
-- License     : BSD-style
--
-- Maintainer  : Geoffrey Mainland <mainland@apeiron.net>
-- Stability   : experimental
-- Portability : non-portable

module Data.Array.Nikola.Util.Quote (
    QuasiQuoter(..),
    dataToQa,
    dataToExpQ,
    dataToPatQ
  ) where

#if MIN_VERSION_template_haskell(2,7,0)
import Language.Haskell.TH.Quote (QuasiQuoter(..),
                                  dataToQa,
                                  dataToExpQ,
                                  dataToPatQ)
#else /* !MIN_VERSION_template_haskell(2,7,0) */
import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH.Syntax
#endif /* !MIN_VERSION_template_haskell(2,7,0) */

#if !MIN_VERSION_template_haskell(2,7,0)
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
          con = mkCon (mkConName mod occ)
            where
              mod :: String
              mod = (tyconModule . dataTypeName . dataTypeOf) t

              occ :: String
              occ = showConstr constr

              mkConName :: String -> String -> Name
              mkConName "Prelude" "(:)" = Name (mkOccName ":") NameS
              mkConName "Prelude" "[]"  = Name (mkOccName "[]") NameS
              mkConName "Prelude" "()"  = Name (mkOccName "()") NameS

              mkConName "Prelude" s@('(' : ',' : rest) = go rest
                where
                  go :: String -> Name
                  go (',' : rest) = go rest
                  go ")"          = Name (mkOccName s) NameS
                  go _            = Name (mkOccName occ) (NameQ (mkModName mod))

              mkConName "GHC.Real" ":%" = mkNameG_d "base" "GHC.Real" ":%"

              mkConName mod occ = Name (mkOccName occ) (NameQ (mkModName mod))

          conArgs :: [Q q]
          conArgs = gmapQ (dataToQa mkCon mkLit appCon antiQ) t

      Just y -> y

-- | 'dataToExpQ' converts a value to a 'Q Exp' representation of the same
-- value. It takes a function to handle type-specific cases.
dataToExpQ  ::  Data a
            =>  (forall b . Data b => b -> Maybe (Q Exp))
            ->  a
            ->  Q Exp
dataToExpQ = dataToQa conE litE (foldl appE)

-- | 'dataToPatQ' converts a value to a 'Q Pat' representation of the same
-- value. It takes a function to handle type-specific cases.
dataToPatQ  ::  Data a
            =>  (forall b . Data b => b -> Maybe (Q Pat))
            ->  a
            ->  Q Pat
dataToPatQ = dataToQa id litP conP
#endif /* !MIN_VERSION_template_haskell(2,7,0) */
