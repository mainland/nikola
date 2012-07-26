{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : Data.Array.Nikola.Util.Pretty
-- Copyright   : (c) Geoffrey Mainland 2012
-- License     : BSD-style
--
-- Maintainer  : Geoffrey Mainland <mainland@apeiron.net>
-- Stability   : experimental
-- Portability : non-portable

module Data.Array.Nikola.Util.Pretty (
    Fixity(..),
    infix_,
    infixl_,
    infixr_,
    defaultFixity,
    addPrec,
    mulPrec,
    appPrec,
    appPrec1,
    infixop,

    embraceStack,
    embraceSep,

    prettyIO
) where

import Control.Monad.Trans (MonadIO(..))
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

embraceStack :: [Doc] -> Doc
embraceStack = embrace stack

embraceSep :: [Doc] -> Doc
embraceSep = embrace sep

embrace :: ([Doc] -> Doc) -> [Doc] -> Doc
embrace combine ds =
    case ds of
      []   -> lbrace <> rbrace
      [d]  -> lbrace <+> d <+> rbrace
      _    -> align $ lbrace <+> combine (semis ds ++ [rbrace])
  where
    semis :: [Doc] -> [Doc]
    semis []     = []
    semis [d]    = [d]
    semis (d:ds) = align d : map (\d -> text ";" <+> align d) ds

prettyIO :: (MonadIO m) => Doc -> m ()
prettyIO = liftIO . putStrLn . pretty 80
