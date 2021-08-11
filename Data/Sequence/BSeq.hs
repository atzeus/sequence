{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types,GADTs, DataKinds, TypeOperators #-}



-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Sequence.BSeq
-- Copyright   :  (c) Atze van der Ploeg 2014
-- License     :  BSD-style
-- Maintainer  :  atzeus@gmail.org
-- Stability   :  provisional
-- Portability :  portable
--
-- A sequence, implemented as a binary tree, good performance when used ephemerally
--
--
-----------------------------------------------------------------------------
module Data.Sequence.BSeq(module Data.SequenceClass,BSeq)  where
import Control.Applicative (pure, (<*>), (<$>))
import Data.Foldable
#if MIN_VERSION_base(4,9,0)
import qualified Data.Semigroup as Semigroup
#endif
import Data.Monoid ((<>))
import Data.Traversable
import Prelude hiding (foldr,foldl)
import Data.SequenceClass

data BSeq a = Empty | Leaf a | Node (BSeq a) (BSeq a)



instance Functor BSeq where
  fmap f = loop where
    loop Empty = Empty
    loop (Leaf x) = Leaf (f x)
    loop (Node l r) = Node (loop l) (loop r)

instance Foldable BSeq where
  foldl f = loop where
    loop i s = case viewl s of
          EmptyL -> i
          h :< t -> loop (f i h) t
  foldr f i s = foldr f i (reverse $ toRevList s)
    where toRevList s = case viewl s of
           EmptyL -> []
           h :< t -> h : toRevList t

instance Traversable BSeq where
  traverse f = loop where
    loop Empty = pure Empty
    loop (Leaf x) = Leaf <$> f x
    loop (Node l r) = Node <$> loop l <*> loop r

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

instance Sequence BSeq where
  empty     = Empty
  singleton = Leaf
  Empty      >< r = r
  l          >< Empty = l
  (Node l r) >< z = Node l (Node r z)
  (Leaf x)   >< z = Node (Leaf x) z
  viewl Empty               = EmptyL
  viewl (Leaf x)            = x :< Empty
  viewl (Node (Leaf x) r)   = x :< r
