-- Copyright (c) 2006-2011
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

--------------------------------------------------------------------------------
-- |
-- Module      :  Nikola.Quote
-- Copyright   :  (c) Harvard University 2006-2011
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu
--
--------------------------------------------------------------------------------

{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Nikola.Quote (
    QuasiQuoter(..),
    dataToQa,
    dataToExpQ,
    dataToPatQ
  ) where

import Data.Generics
import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH.Syntax

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
