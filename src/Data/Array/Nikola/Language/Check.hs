-- Copyright (c) 2009-2010
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

{-# LANGUAGE ScopedTypeVariables #-}

module Data.Array.Nikola.Language.Check (
  MonadCheck(..),
  check,
  checkScalar,
  checkVector,
  checkMatch,
  match
  ) where

import Control.Applicative
import Control.Monad (when)
import Data.List (foldl')
import qualified Data.Map as Map
import Text.PrettyPrint.Mainland

import Data.Array.Nikola.Language.Syntax

class (Functor m, Applicative m, Monad m) => MonadCheck m where
    lookupVar  :: Var -> m Tau
    extendVars :: [(Var, Tau)] -> m a -> m a

check :: forall m . MonadCheck m => DExp -> m Tau
check (VarE v) = lookupVar v

check (DelayedE _) =
    error "check: encountered DelayedE"

check (LetE v tau e1 e2) = do
    tau' <- check e1
    when (tau' /= tau) $
        faildoc $ text "Type mis-match in let binding for" <+> ppr v
    extendVars [(v, tau)] $ check e2

check (LamE vtaus e) = do
    tau <-  extendVars vtaus $
            check e
    return $ FunT (map snd vtaus) tau

check (AppE f es) = do
    (rhos1, rho) <- checkFun f
    rhos2        <- mapM check es
    let phi      =  match rhos2
    when (length rhos1 /= length rhos2) $
        faildoc $ text "Expected" <+> ppr (length rhos1) <+>
                  text "arguments but saw" <+> ppr (length rhos2)
    mapM_ (uncurry checkEqual) (map phi rhos1 `zip` rhos2)
    return (phi rho)

check (BoolE _)  = return BoolT
check (Int32E _) = return Int32T
check (FloatE _) = return FloatT

check (UnopE op e)      = checkUnop op e
check (BinopE op e1 e2) = checkBinop op e1 e2

check (IfteE teste thene elsee) = do
    tau_test <- check teste
    when (tau_test /= BoolT) $ do
        faildoc $ text "Expected boolean type but got" <+> ppr tau_test
    tau_thene <- check thene
    tau_elsee <- check elsee
    when (tau_thene /= tau_elsee) $ do
        faildoc $ text "Type mismatch in if-then-else" <+>
                  ppr tau_thene <+> text "/=" <+> ppr tau_elsee
    return tau_thene

check (MapE f e) = do
    (tau1, n) <- checkVector e
    tau2      <- checkFunType f [tau1] >>=
                 checkScalarType
    return $ vectorT tau2 n

check (MapME f v1 v2) = do
    (tau1, _) <- checkVector v1
    (tau2, _) <- checkVector v2
    tau3      <- checkFunType f [tau1] >>=
                 checkScalarType
    checkEqual tau3 tau2
    return UnitT

check (PermuteE xs is) = do
    (tau1, n1) <- checkVector xs
    (tau2, n2) <- checkVector is
    checkEqual tau2 Int32T
    return $ vectorT tau1 (nmin n1 n2)

check (PermuteME xs is xs') = do
    (tau1, _) <- checkVector xs
    (tau2, _) <- checkVector is
    (tau3, _) <- checkVector xs'
    checkEqual tau2 Int32T
    checkEqual tau3 tau1
    return UnitT

check (ZipWithE f e1 e2) = do
    (tau1, n1) <- checkVector e1
    (tau2, n2) <- checkVector e2
    tau3       <- checkFunType f [tau1, tau2] >>=
                  checkScalarType
    return $ vectorT tau3 (nmin n1 n2)

check (ZipWith3E f e1 e2 e3) = do
    (tau1, n1) <- checkVector e1
    (tau2, n2) <- checkVector e2
    (tau3, n3) <- checkVector e3
    tau4       <- checkFunType f [tau1, tau2, tau3] >>=
                  checkScalarType
    return $ vectorT tau4 (nminimum [n1, n2, n3])

check (ZipWith3ME f xs ys zs results) = do
    (tau1, _) <- checkVector xs
    (tau2, _) <- checkVector ys
    (tau3, _) <- checkVector zs
    (tau4, _) <- checkVector results
    tau5      <- checkFunType f [tau1, tau2, tau3] >>=
                 checkScalarType
    checkEqual tau4 tau5
    return UnitT

check (ScanE f z e) = do
    tau1      <- checkScalar z
    (tau2, n) <- checkVector e
    checkEqual tau1 tau2
    tau3      <- checkFunType f [tau1, tau1] >>=
                 checkScalarType
    checkEqual tau1 tau3
    return $ vectorT tau1 n

check (BlockedScanME f z xs) = do
    tau1      <- checkScalar z
    (tau2, n) <- checkVector xs
    checkEqual tau1 tau2
    tau3      <- checkFunType f [tau1, tau1] >>=
                 checkScalarType
    checkEqual tau1 tau3
    return $ vectorT tau1 (n `div` (2*fromInteger threadBlockWidth))

check (BlockedNacsME f z xs) = do
    tau1      <- checkScalar z
    (tau2, n) <- checkVector xs
    checkEqual tau1 tau2
    tau3      <- checkFunType f [tau1, tau1] >>=
                 checkScalarType
    checkEqual tau1 tau3
    return $ vectorT tau1 (n `div` (2*fromInteger threadBlockWidth))

check (BlockedAddME xs sums) = do
    (tau1, _) <- checkVector xs
    (tau2, _) <- checkVector sums
    checkEqual tau1 tau2
    return UnitT

checkUnop :: MonadCheck m => Unop -> DExp -> m Tau
checkUnop _ e = checkScalar e

checkBinop :: forall m . MonadCheck m
           => Binop
           -> DExp
           -> DExp
           -> m Tau
checkBinop op e1 e2 = go op
  where
    go :: Binop -> m Tau
    go Leq = checkEqOp
    go Lne = checkEqOp
    go Lgt = checkEqOp
    go Lge = checkEqOp
    go Llt = checkEqOp
    go Lle = checkEqOp
    go _   = checkMatch e1 e2

    checkEqOp :: m Tau
    checkEqOp = do
        checkMatch e1 e2
        return BoolT

checkScalar :: MonadCheck m => DExp -> m Tau
checkScalar e = check e >>= checkScalarType

checkVector :: MonadCheck m => DExp -> m (Tau, N)
checkVector e = check e >>= checkVectorType

checkMatch :: MonadCheck m => DExp -> DExp -> m Tau
checkMatch e1 e2 = do
    rho1 <- check e1
    rho2 <- check e2
    checkEqual rho1 rho2
    return rho1

checkFun :: MonadCheck m
         => DExp
         -> m ([Tau], Tau)
checkFun f = do
    ftau <- check f
    case ftau of
      FunT taus tau -> return (taus, tau)
      _ -> faildoc $
           text "Expected function type but got" <+>
           ppr ftau

checkEqual :: MonadCheck m => Tau -> Tau -> m ()
checkEqual rho1 rho2 =
    when (rho1 /= rho2) $
        faildoc $ text "Expected type" <+> ppr rho1 <+>
                  text "but got" <+> ppr rho2

checkScalarType :: MonadCheck m => Tau -> m Tau
checkScalarType UnitT    = return UnitT
checkScalarType BoolT    = return BoolT
checkScalarType Int32T   = return Int32T
checkScalarType FloatT   = return FloatT
checkScalarType rho      = faildoc $
                           text "Expected scalar type but got" <+>
                           ppr rho

checkVectorType :: MonadCheck m => Tau -> m (Tau, N)
checkVectorType (ArrayT tau [n] []) = return (tau, n)
checkVectorType rho                 = faildoc $
                                      text "Expected vector type but got" <+>
                                      ppr rho

checkFunType :: MonadCheck m
             => DExp
             -> [Tau]
             -> m Tau
checkFunType e taus' = do
    (taus, tau) <- checkFun e
    when (length taus /= length taus' || not (all (uncurry (==)) (taus `zip` taus'))) $
        faildoc $
          text "Expected type" <+>
          parens (commasep (map ppr taus')) <+> text "-> alpha" <+>
          text "but got" <+>
          parens (commasep (map ppr taus)) <+> text "-> alpha"
    return tau

-- When applying a function, we need to translate type-level numbers in the
-- function's context to type-level numbers in the context in which the function
-- is applied. @match@ produces the phi function that does this translation, as
-- described in the paper.
match :: [Tau] -> Tau -> Tau
match taus = phi
  where
    phi :: Tau -> Tau
    phi (ArrayT tau sh ptch) =
        ArrayT tau (map phiN sh) (map phiN ptch)

    phi (FunT rhos rho) =
        FunT (map phi rhos) (phi rho)

    phi rho =
        rho

    phiN :: N -> N
    phiN (NMin ns) =
        NMin $ map phiN ns

    phiN n =
        case Map.lookup n nmap of
          Nothing -> errordoc $ text "No translation for:" <+> ppr n
          Just n' -> n'

    nmap :: Map.Map N N
    nmap = go Map.empty 0 taus

    go :: Map.Map N N -> Int -> [Tau] -> Map.Map N N
    go m _ [] =
        m

    go m pi (ArrayT _ dims pitches : taus) =
        go (foldl' insert m kvs) (pi+1) taus
      where
        kvs = [NDim d (ParamIdx pi) | d <- [0..] ] `zip` dims
              ++
              [NPitch d (ParamIdx pi) | d <- [0..] ] `zip` pitches

    go _ _ (FunT {} : _) =
        error "The impossible happened: an embedded higher-order function!"

    go m pi (_ : taus) =
        go m (pi+1) taus

    insert :: Ord k => Map.Map k v -> (k, v) -> Map.Map k v
    insert m (k, v) = Map.insert k v m
