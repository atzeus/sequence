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
import Data.Foldable
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

instance Sequence q => Foldable (ToCatQueue q) where
  foldl f = loop where
    loop i s = case viewl s of
          EmptyL -> i
          h :< t -> loop (f i h) t
  foldr f i s = foldr f i (reverse $ toRevList s)
    where toRevList s = case viewl s of
           EmptyL -> []
           h :< t -> h : toRevList t

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

