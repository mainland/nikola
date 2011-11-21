{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- Copyright (c) 2008-2009
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
-- Module      :  Control.Monad.Exception
-- Copyright   :  (c) Harvard University 2008-2009
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu
--
--------------------------------------------------------------------------------

module Control.Monad.Exception where

import Control.Exception hiding (catch)
import Control.Monad.Error
import Data.Typeable
import Text.PrettyPrint.Mainland

class MonadError SomeException m => MonadException m where
    throwException :: Exception e => e -> m a
    throwException = throwError . toException

    catchException  ::  Exception e
                    =>  m a
                    ->  (e -> m a)
                    ->  m a
    catchException m h =
        m `catchError` \e -> dispatchException e h

failDoc :: MonadException m => Doc -> m a
failDoc doc = throwException $ DocMsgException doc

dispatchException  ::  (MonadException m, Exception e)
                   =>  SomeException
                   ->  (e -> m a)
                   ->  m a
dispatchException e h =
    case fromException e of
      Just e' ->  h e'
      Nothing ->  throwException e

instance Error SomeException where
    noMsg      = toException $ NoMsgException
    strMsg msg = toException $ StrMsgException msg

data NoMsgException = NoMsgException
  deriving (Typeable)

instance Show NoMsgException where
    show NoMsgException = ""

instance Exception NoMsgException

data StrMsgException = StrMsgException [Char]
  deriving (Typeable)

instance Show StrMsgException where
    show (StrMsgException msg) = msg

instance Exception StrMsgException

data DocMsgException = DocMsgException Doc
  deriving (Typeable)

instance Show DocMsgException where
    show (DocMsgException doc) = show doc

instance Exception DocMsgException

data PanicException = PanicException String
  deriving (Typeable)

instance Show PanicException where
    show (PanicException msg) = "panic: " ++ msg

instance Exception PanicException

panic :: (MonadException m) => Doc -> m a
panic = throwException . PanicException . pretty 80

data InternalErrorException = InternalErrorException String
  deriving (Typeable)

instance Show InternalErrorException where
    show (InternalErrorException msg) = "internal error: " ++ msg

instance Exception InternalErrorException

internalErr :: Doc -> a
internalErr = throw . InternalErrorException . pretty 80
