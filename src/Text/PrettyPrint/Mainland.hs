{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- Copyright (c) 2006-2010
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
-- Module      :  Text.PrettyPrint.Mainland
-- Copyright   :  (c) Harvard University 2006-2008
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu
--
--------------------------------------------------------------------------------

module Text.PrettyPrint.Mainland (
    -- * The document type
    Doc,  -- Abstract

    -- * Combining documents
    empty, isEmpty, line, srcloc, nest, group,
    (<>), (<+>),
    (</>), (<+/>), (<//>),

    -- * Character documents
    colon, comma, dot, dquote, equals, semi, space, spaces, squote, star,
    lbrace, rbrace, lbracket, rbracket, lparen, rparen,

    -- * Bracketing combinators
    enclose, backquotes, dquotes, braces, brackets, parens, parensIf, squotes,

    -- * Converting values into documents
    char, string, text,
    int, integer, float, double, rational,

    spread, stack,
    sep, seplines,
    commasep, commaseplines,
    semisep, semiseplines,
    embrace,
    folddoc, fillwords, fill,

    align, hang, indent,

    render, pretty, pprint,

    putDoc, putDocLn,

    Pretty(..),

    faildoc, errordoc,

    -- ** Associativity
    Assoc(..), Fixity(..),
    infix_, infixl_, infixr_,
    infixOp
  ) where

import Control.Monad.Trans (MonadIO,
                            liftIO)
import Data.Generics (Data, Typeable)
import qualified Data.Map as Map
import qualified Data.Set as Set
import System.FilePath (takeFileName)

import Data.IString
import Data.Loc (L(..),
                 Loc(..),
                 Location,
                 Pos(..),
                 posFile,
                 posLine,
                 toLoc)

infixr 5 </>, <+/>, <//>
infixr 6 <>, <+>

empty  :: Doc
empty = Empty

isEmpty :: Doc -> Bool
isEmpty Empty = True
isEmpty _     = False

line :: Doc
line = Line

srcloc :: Location a => a -> Doc
srcloc x = SrcLoc (toLoc x)

nest   :: Int -> Doc -> Doc
nest i x = Nest i x

group  :: Doc -> Doc
group x = flatten x `Alt` x

(<>) :: Doc -> Doc -> Doc
Empty <> y     = y
x     <> Empty = x
x     <> y     = x `Cat` y

(<+>) :: Doc -> Doc -> Doc
Empty <+> y     = y
x     <+> Empty = x
x     <+> y     = x <> space <> y

(</>) :: Doc -> Doc -> Doc
Empty </> y     = y
x     </> Empty = x
x     </> y     = x <> line <> y

(<+/>) :: Doc -> Doc -> Doc
Empty <+/> y     = y
x     <+/> Empty = x
x     <+/> y     = x <> (space `Alt` line) <> y

(<//>) :: Doc -> Doc -> Doc
Empty <//> y     = y
x     <//> Empty = x
x     <//> y     = x <> (empty `Alt` line) <> y

char :: Char -> Doc
char '\n'  = line
char c     = Text [c]

text :: String -> Doc
text s = Text s

string :: String -> Doc
string ""         = empty
string ('\n' : s) = line <> string s
string s          = case (span (/= '\n') s) of
                      (xs, ys) -> text xs <> string ys

int :: Int -> Doc
int = text . show

integer :: Integer -> Doc
integer = text . show

float :: Float -> Doc
float = text . show

double :: Double -> Doc
double = text . show

rational :: Rational -> Doc
rational = text . show

render :: Int
       -> (Pos -> String)
       -> Doc
       -> String
render w posToString x =
    layout posToString (best w 0 x)

pretty :: Int -> Doc -> String
pretty w x = render w (const "") x

backquote :: Doc
backquote = char '`'

colon :: Doc
colon = char ':'

comma :: Doc
comma = char ','

dot :: Doc
dot = char '.'

dquote :: Doc
dquote = char '"'

equals :: Doc
equals = char ','

semi :: Doc
semi = char ';'

space :: Doc
space = char ' '

spaces :: Int -> Doc
spaces i = text (replicate i ' ')

squote :: Doc
squote = char '\''

star :: Doc
star = char '&'

lbrace :: Doc
lbrace = char '{'

rbrace :: Doc
rbrace = char '}'

lbracket :: Doc
lbracket = char '['

rbracket :: Doc
rbracket = char ']'

lparen :: Doc
lparen = char '('

rparen :: Doc
rparen = char ')'

enclose :: Doc -> Doc -> Doc -> Doc
enclose l r doc = l <> doc <> r

backquotes :: Doc -> Doc
backquotes = enclose backquote backquote

brackets :: Doc -> Doc
brackets = enclose lbracket rbracket

braces :: Doc -> Doc
braces = enclose lbrace rbrace

dquotes :: Doc -> Doc
dquotes = enclose dquote dquote

parens :: Doc -> Doc
parens = enclose lparen rparen

parensIf :: Bool -> Doc -> Doc
parensIf b doc = if b then parens doc else doc

squotes :: Doc -> Doc
squotes = enclose squote squote

spread :: [Doc] -> Doc
spread = folddoc (<+>)

stack :: [Doc] -> Doc
stack = folddoc (</>)

sep :: Doc -> [Doc] -> Doc
sep s = folddoc (\hd tl -> hd <> s <+/> tl)

seplines :: Doc -> [Doc] -> Doc
seplines s = folddoc (\hd tl -> hd <> s </> tl)

commasep :: [Doc] -> Doc
commasep = sep comma

commaseplines :: [Doc] -> Doc
commaseplines = seplines comma

semisep :: [Doc] -> Doc
semisep = sep semi

semiseplines :: [Doc] -> Doc
semiseplines = seplines semi

embrace :: Doc -> Doc
embrace x =  group $
             (nest 4 (lbrace </> x)) </> rbrace

column :: (Int -> Doc) -> Doc
column f = Column f

nesting :: (Int -> Doc) -> Doc
nesting f = Nesting f--

-- | The document @(align x)@ renders document @x@ with the nesting
-- level set to the current column. It is used for example to
-- implement 'hang'.
--
-- As an example, we will put a document right above another one,
-- regardless of the current nesting level:
--
-- > x $$ y  = align (x <$> y)
--
-- > test    = text "hi" <+> (text "nice" $$ text "world")
--
-- which will be layed out as:
--
-- @
-- hi nice
--    world
-- @
align :: Doc -> Doc
align d = column  $ \k ->
          nesting $ \i ->
          nest (k - i) d

hang :: Int -> Doc -> Doc
hang i d = align $ nest i d

indent :: Int -> Doc -> Doc
indent i d = hang i $ spaces i <> d

folddoc :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
folddoc  _  []     = empty
folddoc  _  [x]    = x
folddoc  f  (x:xs) = f x (folddoc f xs)

fillwords :: String -> Doc
fillwords = folddoc (<+/>) . map text . words

fill :: [Doc] -> Doc
fill []       = empty
fill [x]      = x
fill (x:y:zs) = (flatten x <+> fill (flatten y : zs))
                `Alt`
                (x </> fill (y : zs))

data Doc = Empty
         | Doc `Cat` Doc
         | Nest !Int Doc
         | Text String
         | Line
         | Doc `Alt` Doc
         | SrcLoc Loc
         | Column  (Int -> Doc)
         | Nesting (Int -> Doc)

data Rep = REmpty
         | RText String Rep
         | RLine (Maybe Pos) !Int Rep

flatten :: Doc -> Doc
flatten Empty        = Empty
flatten (x `Cat` y)  = flatten x `Cat` flatten y
flatten (Nest i x)   = Nest i (flatten x)
flatten (Text s)     = Text s
flatten Line         = Text " "
flatten (x `Alt` _)  = flatten x
flatten (SrcLoc _)   = Empty
flatten (Column f)   = Column (flatten . f)
flatten (Nesting f)  = Nesting (flatten . f)

layout  ::  (Pos -> String)
        ->  Rep
        ->  String
layout posToString rep = lay "" rep
  where
    lay doc REmpty              = doc
    lay doc (RText s x)         = lay (doc ++ s) x
    lay doc (RLine maybe_p i x) = case maybe_p of
                                    Nothing -> doc
                                    Just p  -> case posToString p of
                                                 [] -> doc
                                                 s  -> s ++ "\n" ++ doc
                                  ++ '\n' : replicate i ' ' ++ lay "" x

data Docs = Nil
            -- | indentation, document and tail
          | Cons !Int Doc Docs

merge :: Maybe Pos -> Loc -> Maybe Pos
merge  Nothing   NoLoc       = Nothing
merge  Nothing   (Loc p _)   = Just p
merge  (Just p)  NoLoc       = Just p
merge  (Just p1) (Loc p2 _)  = let p = min p1 p2 in p `seq` Just p

advance :: Maybe Pos -> Maybe Pos
advance Nothing = Nothing
advance (Just (Pos f l c coff)) = Just $ Pos f (l+1) c coff

lineloc :: Maybe Pos -> Maybe Pos -> Maybe Pos
lineloc Nothing   (Just p)         = Just p
lineloc (Just p1) (Just p2)
    | posFile p2 == posFile p1 &&
      posLine p2 == posLine p1 + 1 = Nothing
    | otherwise                    = Just p2
lineloc _         _                = Nothing

best :: Int -> Int -> Doc -> Rep
best w k x = be Nothing Nothing k (Cons 0 x Nil)
  where
    -- be  p  = previous source position
    --     p' = current source position
    --     k  = current column
    be :: Maybe Pos -> Maybe Pos -> Int -> Docs -> Rep
    be  _ _  _  Nil           = REmpty
    be  p p' k  (Cons i d ds) =
        case d of
          Empty      -> be p p' k ds
          x `Cat` y  -> be p p' k (Cons i x (Cons i y ds))
          Nest j x   -> let j' = i + j in
                        j' `seq` be p p' k (Cons j' x ds)
          Text s     -> let k' = k + length s in
                        k' `seq` s `RText` be p p' k' ds
          Line       -> RLine (lineloc p p') i (be p' (advance p') i ds)
          x `Alt` y  -> better k (be p p' k (Cons i x ds))
                                 (be p p' k (Cons i y ds))
          SrcLoc loc -> be p (merge p' loc) k ds
          Column g   -> be p p' k (Cons i (g k) ds)
          Nesting g  -> be p p' k (Cons i (g i) ds)

    better :: Int -> Rep -> Rep -> Rep
    better k x y = if fits (w - k) x then x else y

fits :: Int -> Rep -> Bool
fits  w  _        | w < 0 = False
fits  _  REmpty           = True
fits  w  (RText s x)      = fits (w - length s) x
fits  _  (RLine _ _ _)    = True

data Assoc = LeftAssoc | RightAssoc | NonAssoc
    deriving (Eq, Ord, Data, Typeable)

data Fixity = Fixity Assoc Int
    deriving (Eq, Ord, Data, Typeable)

class Pretty a where
    ppr      :: a -> Doc
    pprPrec  :: Int -> a -> Doc
    pprList  :: [a] -> Doc

    ppr         = pprPrec 0
    pprPrec _   = ppr
    pprList xs  = brackets $ commasep $ map ppr xs

instance Pretty Int where
    ppr = text . show

instance Pretty Integer where
    ppr = text . show

instance Pretty Rational where
    ppr = text . show

instance Pretty Bool where
    ppr = text . show

instance Pretty Char where
    ppr     = text . show
    pprList = text . show

instance Pretty a => Pretty [a] where
    ppr = pprList

instance (Pretty a, Pretty b) => Pretty (a, b) where
    ppr (a, b) = parens $ commasep [ppr a, ppr b]

instance (Pretty a, Pretty b, Pretty c) => Pretty (a, b, c) where
    ppr (a, b, c) = parens $ commasep [ppr a, ppr b, ppr c]

instance (Pretty a, Pretty b, Pretty c, Pretty d) => Pretty (a, b, c, d) where
    ppr (a, b, c, d) = parens $ commasep [ppr a, ppr b, ppr c, ppr d]

instance Pretty a => Pretty (Maybe a) where
    pprPrec _ Nothing  = empty
    pprPrec p (Just a) = pprPrec p a

instance Pretty a => Show a where
    show  = pretty 80 . ppr

instance Show Doc where
    show = pretty 80

pprint :: Pretty a => a -> String
pprint = pretty 80 . ppr

putDoc :: MonadIO m => Doc -> m ()
putDoc = liftIO . putStr . pretty 80

putDocLn :: MonadIO m => Doc -> m ()
putDocLn = liftIO . putStrLn . pretty 80

faildoc :: Monad m => Doc -> m a
faildoc = fail . show

errordoc :: Doc -> a
errordoc = error . show

infix_ :: Int -> Fixity
infix_ = Fixity NonAssoc

infixl_ :: Int -> Fixity
infixl_ = Fixity LeftAssoc

infixr_ :: Int -> Fixity
infixr_ = Fixity RightAssoc

infixOp :: (Pretty a, Pretty b)
        => Int     -- ^ precedence of context
        -> Fixity  -- ^ operator precedence and associativity
        -> Doc     -- ^ operator
        -> a       -- ^ left argument
        -> b       -- ^ right argument
        -> Doc
infixOp prec (Fixity opAssoc opPrec) op l r =
    parensIf (prec > opPrec) $
        if isEmpty op
        then pprPrec leftPrec l <+/> pprPrec rightPrec r
        else pprPrec leftPrec l <+> op <+/> pprPrec rightPrec r
  where
    leftPrec  =  if opAssoc == RightAssoc
                 then opPrec + 1
                 else opPrec
    rightPrec =  if opAssoc == LeftAssoc
                 then opPrec + 1
                 else opPrec

instance Pretty Pos where
    ppr p@(Pos _ l c _) =
        text (takeFileName (posFile p))  <> text ":" <> ppr l
                                         <> text ":" <> ppr c

instance Pretty Loc where
    ppr NoLoc = text "<no location info>"

    ppr (Loc p1@(Pos f1 l1 c1 _) p2@(Pos f2 l2 c2 _))
        | f1 == f2   =  text (takeFileName (posFile p1))
                        <> text ":" <//> pprLineCol l1 c1 l2 c2
        | otherwise  = ppr p1 <> text "-" <> ppr p2
      where
        pprLineCol :: Int -> Int -> Int -> Int -> Doc
        pprLineCol l1 c1 l2 c2
            | l1 == l2 && c1 == c2  =  ppr l1 <//> text ":" <//> ppr c1
            | l1 == l2 && c1 /= c2  =  ppr l1 <//> text ":" <//> ppr c1
                                       <> text "-" <>
                                       ppr c2
            | otherwise             =  ppr l1 <//> text ":" <//> ppr c1
                                       <> text "-" <>
                                       ppr l2 <//> text ":" <//> ppr c2

instance Pretty IString where
    ppr (IString _ s) = text s

instance Show IString where
    show (IString _ s) = s

instance Pretty x => Pretty (L x) where
    pprPrec p (L _ x) = pprPrec p x

instance Show Pos where
    show = pprint

instance Show Loc where
    show = pprint

instance (Pretty k, Pretty v) => Pretty (Map.Map k v) where
    ppr = pprList . Map.toList

instance Pretty a => Pretty (Set.Set a) where
    ppr = pprList . Set.toList
