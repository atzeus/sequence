{-# LANGUAGE UndecidableInstances, GADTs,TypeSynonymInstances,FlexibleInstances,Rank2Types #-}



-----------------------------------------------------------------------------
-- |
-- Module      :  Data.SequenceClass
-- Copyright   :  (c) Atze van der Ploeg 2014
-- License     :  BSD-style
-- Maintainer  :  atzeus@gmail.org
-- Stability   :  provisional
-- Portability :  portable
-- A type class for sequences.
--
-- See the package 'type-aligned' for a generalization of this type class sequences.
-- 
-----------------------------------------------------------------------------
module Data.SequenceClass(Sequence(..), ViewL(..), ViewR(..)) where

import Data.Monoid
import Data.Foldable
import qualified Data.Sequence as S

infixr 5 <|
infixl 5 |>
infix 5 ><
{- | A type class for (finite) sequences
 
Minimal complete defention: 'empty' and 'singleton' and ('viewl' or 'viewr') and ('><' or '|>' or '<|')

Instances should satisfy the following laws:

Monoid laws:
> empty >< x == x
> x >< empty == x
> (x >< y) >< z = x >< (y >< z)

Observation laws:
> viewl (singleton e >< s) == e :< s
> viewl empty == EmptyL

The behaviour of '<|','|>', and 'viewr' is implied by the above laws and their default definitions.
-}
class (Functor s, Foldable s) => Sequence s where

  empty     :: s c 
  singleton :: c  -> s c 
  -- | Append two sequences
  (><)       :: s c  -> s c   -> s c 
  -- | View a sequence from the left
  viewl     :: s c  -> ViewL s c 
{- | View a sequence from the right
         
Default definition:
> viewr q = case viewl q of 
>    EmptyL -> EmptyR
>    h :< t -> case viewr t of
>        EmptyR -> empty   :> h
>        p :> l   -> (h <| p) :> l
-}
  viewr     :: s c -> ViewR s c 
{- | Append a single element to the right

Default definition:
> l |> r = l >< singleton r
-}
  (|>)       :: s c -> c  -> s c 
{- | Append a single element to the left

Default definition:
> l <| r = tsingleton l >< r
-}
  (<|)       :: c  -> s c -> s c
  
  l |> r = l >< singleton r
  l <| r = singleton l >< r
  l >< r = case viewl l of
    EmptyL -> r
    h :< t  -> h <| (t >< r)

  viewl q = case viewr q of 
    EmptyR -> EmptyL
    p :> l -> case viewl p of
        EmptyL -> l :< empty
        h :< t   -> h :< (t |> l)

  viewr q = case viewl q of 
    EmptyL -> EmptyR
    h :< t -> case viewr t of
        EmptyR -> empty   :> h
        p :> l   -> (h <| p) :> l

data ViewL s c where
   EmptyL  :: ViewL s c 
   (:<)    :: c  -> s c  -> ViewL s c 

data ViewR s c  where
   EmptyR  :: ViewR s c 
   (:>)     :: s c -> c -> ViewR s c 

 
instance Sequence S.Seq where
 empty = S.empty
 singleton = S.singleton
 (<|) = (S.<|)
 (|>) = (S.|>)
 (><) = (S.><)
 viewl s = case S.viewl s of
   S.EmptyL -> EmptyL
   h S.:< t -> h :< t
 viewr s = case S.viewr s of
   S.EmptyR -> EmptyR
   t S.:> h -> t :> h

instance Sequence [] where
  empty = []
  singleton x = [x]
  (<|) = (:)
  viewl [] = EmptyL
  viewl (h : t) = h :< t 



