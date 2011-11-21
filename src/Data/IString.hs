{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- Copyright (c) 2009
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
-- Module      :  Data.IString
-- Copyright   :  (c) Harvard University 2008-2009
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu
--
--------------------------------------------------------------------------------

module Data.IString (
    IString(..),
    istring,
    istringString
  ) where

import Data.Generics (Data,
                      Typeable)
import Data.IORef (IORef,
                   newIORef,
                   readIORef,
                   writeIORef)
#if __GLASGOW_HASKELL__ >= 608
import Data.String
#endif /* __GLASGOW_HASKELL__ >= 608 */
import qualified Data.Map as Map
import System.IO.Unsafe (unsafePerformIO)

data IString =  -- | Unique identifier and the string itself
                IString {-# UNPACK #-} !Int !String
  deriving (Data, Typeable)

instance Eq IString where
    (IString i1 _) == (IString i2 _) = i1 == i2

instance Ord IString where
    compare (IString i1 _) (IString i2 _) = compare i1 i2

#if __GLASGOW_HASKELL__ >= 608
instance IsString IString where
    fromString = istring
#endif /* __GLASGOW_HASKELL__ >= 608 */

istringString (IString _ s) = s

data IStringEnv = IStringEnv
    {  uniq     :: {-# UNPACK #-} !Int
    ,  istrings :: {-# UNPACK #-} !(Map.Map String IString)
    }

istringEnv :: IORef IStringEnv
istringEnv = unsafePerformIO $ newIORef $
          IStringEnv 1 Map.empty

istring :: String -> IString
istring s = unsafePerformIO $ do
    env <- readIORef istringEnv
    case Map.lookup s (istrings env) of
      Nothing  -> do  let is = IString (uniq env) s
                      writeIORef istringEnv $
                          env  {  uniq     = uniq env + 1,
                                  istrings = Map.insert s is (istrings env)
                               }
                      return is
      Just is  -> return is
