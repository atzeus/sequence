{-# LANGUAGE GADTs #-}



-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Sequence.ToCatQueue
-- Copyright   :  (c) Atze van der Ploeg 2013
-- License     :  BSD-style
-- Maintainer  :  atzeus@gmail.org
-- Stability   :  provisional
-- Portability :  portable
--
-- A purely functional catenable queue representation with
-- that turns takes a purely functional queue and turns in it into
-- a catenable queue, i.e. with the same complexity for '><' as for '|>'
-- Based on Purely functional data structures by Chris Okasaki 
-- section 7.2: Catenable lists
--
-----------------------------------------------------------------------------

module Data.Sequence.ToCatQueue(module Data.SequenceClass,ToCatQueue) where
import Control.Applicative (pure, (<*>), (<$>))
import Data.Foldable
import Data.Traversable
import Prelude hiding (foldr,foldl)
import Data.SequenceClass

-- | The catenable queue type. The first type argument is the 
-- type of the queue we use (|>)
data ToCatQueue q a where
  C0 :: ToCatQueue q a
  CN :: a -> !(q (ToCatQueue q a)) -> ToCatQueue q a

instance Functor q => Functor (ToCatQueue q) where
  fmap f C0 = C0
  fmap f (CN l m) = CN (f l) (fmap (fmap f) m)

instance Foldable q => Foldable (ToCatQueue q) where
  foldl f z C0 = z
  foldl f z (CN x qs) = foldl (foldl f) (f z x) qs
  foldr f z C0 = z
  foldr f z (CN x qs) = x `f` foldr (\q z -> foldr f z q) z qs

instance Sequence q => Sequence (ToCatQueue q) where
 empty       = C0
 singleton a = CN a empty
 C0        >< ys  = ys
 xs        >< C0  = xs
 (CN x q)  >< ys  = CN x (q |> ys)

 viewl C0        = EmptyL
 viewl (CN h t)  = h :< linkAll t
   where 
    linkAll :: Sequence q =>  q (ToCatQueue q a)  -> ToCatQueue q a
    linkAll v = case viewl v of
     EmptyL     -> C0
     CN x q :< t  -> CN x (q `snoc` linkAll t)
    snoc q C0  = q
    snoc q r   = q |> r

instance Traversable q => Traversable (ToCatQueue q) where
  traverse f C0 = pure C0
  traverse f (CN x qs) = CN <$> f x <*> traverse (traverse f) qs
