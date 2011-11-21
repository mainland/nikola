{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- Copyright (c) 2007-2009
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
-- Module      :  Control.Monad.ContextException
-- Copyright   :  (c) Harvard University 2007-2009
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu
--
--------------------------------------------------------------------------------

module Control.Monad.ContextException where

import Prelude hiding (catch)

import Control.Exception
import Control.Monad.Error
import Data.Typeable

import Control.Monad.Exception
import Data.Loc
import Text.PrettyPrint.Mainland

data ErrorContext  =  ErrorContext !Doc
                   |  LocErrorContext !Loc !Doc
  deriving (Typeable)

data ContextException = ContextException [ErrorContext] SomeException
  deriving (Typeable)

instance Exception ContextException where

instance Show ContextException where
    show (ContextException ctx e) = pretty 80 $
        case locs of
          []  ->  text (show e) </> stack docs
          _   ->  nest 4 $  ppr (head locs) <> text ":"
                            </> string (show e)
                            </> stack docs
      where
        locs = [loc | LocErrorContext loc _ <- ctx]
        docs = map contextDoc ctx

        contextDoc :: ErrorContext -> Doc
        contextDoc  (LocErrorContext _ doc)  = doc
        contextDoc  (ErrorContext doc)       = doc

data ContextEnv = ContextEnv  {  maxContext    ::  !Int,
                                 errorContext  ::  ![ErrorContext]
                              }

emptyContextEnv :: ContextEnv
emptyContextEnv = ContextEnv  {  maxContext    = 4,
                                 errorContext  = []
                              }

class MonadError SomeException m => MonadContextException m where
    getContextEnv  :: m ContextEnv
    putContextEnv  :: ContextEnv -> m ()

    getsContextEnv :: (ContextEnv -> a) -> m a
    getsContextEnv f = getContextEnv >>= \s -> return (f s)

    modifyContextEnv :: (ContextEnv -> ContextEnv) -> m ()
    modifyContextEnv f = getContextEnv >>= \s -> putContextEnv (f s)

    setMaxContext :: Int -> m ()
    setMaxContext depth =
        modifyContextEnv $ \s -> s { maxContext = depth }

    withContext :: Doc -> m a -> m a
    withContext doc m = do
        modifyContextEnv $ \s ->
            let ctx = errorContext s
            in
              ctx `seq` s { errorContext = ErrorContext doc : ctx }
        a <- m
        modifyContextEnv $ \s ->
            let ctx = errorContext s
            in
              ctx `seq` s { errorContext = tail ctx }
        return a

    withLocContext :: Located a => a -> Doc -> m b -> m b
    withLocContext a doc m = do
        modifyContextEnv $ \s ->
            let ctx = errorContext s
            in
              ctx `seq` s { errorContext = LocErrorContext loc doc : ctx }
        a <- m
        modifyContextEnv $ \s ->
            let ctx = errorContext s
            in
              ctx `seq` s { errorContext = tail ctx }
        return a
      where
        loc :: Loc
        loc  = getLoc a

    getErrContext :: m [ErrorContext]
    getErrContext = do
        ctx    <- getsContextEnv errorContext
        depth  <- getsContextEnv maxContext
        return $ take depth ctx

    throwExceptionAt  ::  (Location l, Exception e)
                      =>  l
                      ->  e
                      ->  m a
    throwExceptionAt loc e =
        (throwError . toException) (ContextException ctx (toException e))
      where
        ctx = [LocErrorContext (toLoc loc) empty]

    catchContextException  ::  (Exception e)
                           =>  m a
                           ->  ([ErrorContext] -> e -> m a)
                           ->  m a
    catchContextException m h =
        m `catchError` \e ->
            case fromException e of
              Just (ContextException ctx e'') ->  dispatchException e'' (h ctx)
              Nothing ->                          dispatchException e (h [])

instance MonadContextException m => MonadException m where
    throwException e = do
        ctx <- getErrContext
        (throwError . toException) (ContextException ctx (toException e))

    catchException m h =
        m `catchError` \e ->
            case fromException e of
              Just (ContextException _ e'') ->  dispatchException e'' h
              Nothing ->                        dispatchException e h
