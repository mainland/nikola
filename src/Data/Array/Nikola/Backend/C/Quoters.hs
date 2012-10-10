-- |
-- Module      : Data.Array.Nikola.Backend.C.Quoters
-- Copyright   : (c) Geoffrey Mainland 2012
-- License     : BSD-style
--
-- Maintainer  : Geoffrey Mainland <mainland@apeiron.net>
-- Stability   : experimental
-- Portability : non-portable

module Data.Array.Nikola.Backend.C.Quoters (
   cedeclCU,
   cdeclCU,
   cexpCU,
   cstmCU
  ) where


import qualified Language.C.Quote.CUDA as CU

cedeclCU = CU.cedecl
cdeclCU  = CU.cdecl
cexpCU   = CU.cexp
cstmCU   = CU.cstm
