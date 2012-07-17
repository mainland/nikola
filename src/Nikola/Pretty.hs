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
--
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

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Nikola.Pretty (
    Fixity,
    infix_,
    infixl_,
    infixr_,
    defaultFixity,
    addPrec,
    mulPrec,
    appPrec,
    appPrec1,
    infixop,

    embrace
) where

import Text.PrettyPrint.Mainland

data Fixity = Fixity FixityDirection Int
  deriving (Eq, Ord)

data FixityDirection = InfixL | InfixR | Infix
  deriving (Eq, Ord)

infix_ :: Int -> Fixity
infix_ = Fixity Infix

infixl_ :: Int -> Fixity
infixl_ = Fixity InfixL

infixr_ :: Int -> Fixity
infixr_ = Fixity InfixR

defaultFixity :: Fixity
defaultFixity = infixr_ 9

addPrec :: Int
addPrec = 6

mulPrec :: Int
mulPrec = 7

appPrec :: Int
appPrec = 10

appPrec1 :: Int
appPrec1 = appPrec + 1

infixop :: (Pretty a, Pretty b)
        => Int    -- Precedence of context
        -> Fixity -- Fixity of the operator
        -> Doc    -- Operator
        -> a      -- Left argument
        -> b      -- Right argument
        -> Doc
infixop prec (Fixity opAssoc opPrec) op l r =
    parensIf (prec > opPrec) $
    pprPrec leftPrec l <+> op <+/> pprPrec rightPrec r
  where
    leftPrec   | opAssoc == InfixR  = opPrec + 1
               | otherwise          = opPrec
    rightPrec  | opAssoc == InfixL  = opPrec + 1
               | otherwise          = opPrec

embrace ::[Doc] -> Doc
embrace ds =
    case ds of
      [] ->  lbrace <> rbrace
      [d] -> lbrace <+> d <+> rbrace
      _ ->   lbrace </> stack (semis ds) </> rbrace
  where
    semis :: [Doc] -> [Doc]
    semis []     = []
    semis [d]    = [d]
    semis (d:ds) = d <> semi : semis ds
