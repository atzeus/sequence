{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types,GADTs, DataKinds, TypeOperators #-}
{-# LANGUAGE DeriveTraversable #-}
#if __GLASGOW_HASKELL__ < 710
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
#endif



-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Sequence.BSeq
-- Copyright   :  (c) Atze van der Ploeg 2014
-- License     :  BSD-style
-- Maintainer  :  atzeus@gmail.org
-- Stability   :  provisional
-- Portability :  portable
--
-- A catenable qeueue, implemented as a binary tree,
-- with good amortized performance when used ephemerally.
--
--
-----------------------------------------------------------------------------
module Data.Sequence.BSeq(module Data.SequenceClass,BSeq)  where
import Control.Applicative hiding (empty)
import Data.Foldable
import Data.Monoid ((<>))
import Data.Traversable
import qualified Text.Read as TR
#if MIN_VERSION_base(4,9,0)
import qualified Data.Semigroup as Semigroup
import Data.Functor.Classes (Show1 (..))
#endif
import Data.Function (on)
import Prelude hiding (foldr,foldl)
import Data.SequenceClass

-- | A catenable queue intended for ephemeral use.
data BSeq a = Empty | Leaf a | Node (BSeq a) (BSeq a)
-- Invariant: Neither child of a Node may be Empty.
  deriving (Functor, Foldable, Traversable)

#if MIN_VERSION_base(4,9,0)
instance Semigroup.Semigroup (BSeq a) where
  (<>) = (><)
#endif
instance Monoid (BSeq a) where
  mempty = empty
#if MIN_VERSION_base(4,9,0)
  mappend = (Semigroup.<>)
#else
  mappend = (><)
#endif

instance Show a => Show (BSeq a) where
    showsPrec p xs = showParen (p > 10) $
        showString "fromList " . shows (toList xs)

#if MIN_VERSION_base(4,9,0)
instance Show1 BSeq where
  liftShowsPrec _shwsPrc shwList p xs = showParen (p > 10) $
        showString "fromList " . shwList (toList xs)
#endif

instance Read a => Read (BSeq a) where
    readPrec = TR.parens $ TR.prec 10 $ do
        TR.Ident "fromList" <- TR.lexP
        xs <- TR.readPrec
        return (fromList xs)

    readListPrec = TR.readListPrecDefault

instance Eq a => Eq (BSeq a) where
  (==) = (==) `on` toList

instance Ord a => Ord (BSeq a) where
  compare = compare `on` toList

instance Sequence BSeq where
  empty     = Empty
  singleton = Leaf
  Empty      >< r = r
  l          >< Empty = l
  Node l r   >< z = Node l (Node r z)
  l@(Leaf _) >< z = Node l z
  viewl Empty         = EmptyL
  viewl (Leaf x)      = x :< Empty
  viewl (Node l r)    = case viewl l of
    EmptyL -> error "Invariant failure"
    x :< l' -> (x :<) $! l' >< r
  fromList [] = Empty
  fromList [x] = Leaf x
  fromList (x : xs) = Node (Leaf x) (fromList xs)
