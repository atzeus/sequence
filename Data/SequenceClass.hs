{-# LANGUAGE UndecidableInstances, GADTs,TypeSynonymInstances,FlexibleInstances,Rank2Types #-}
{-# LANGUAGE StandaloneDeriving #-}



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
-- See the package type-aligned for a generalization of this type class sequences.
-- 
-----------------------------------------------------------------------------
module Data.SequenceClass(Sequence(..), ViewL(..), ViewR(..)) where

import Data.Monoid
import Data.Foldable
import qualified Data.Sequence as S

infixr 5 <|
infixl 5 |>
infix 5 ><
infixl 9 :<
infixr 9 :>
{- | A type class for (finite) sequences
 

Instances should be /free monoids/
(<http://comonad.com/reader/2015/free-monoids-in-haskell/ ignoring issues with infinite and partially defined structures>),
just like lists, with @singleton@ as the canonical injection and @foldMap@
factoring functions.  In particular, they should satisfy the following laws:

@Semigroup@ and @Monoid@ laws:

> (><) == (Data.Semigroup.<>)
> empty == mempty

In particular, this requires that

> empty >< x == x
> x >< empty == x
> (x >< y) >< z = x >< (y >< z)

@FoldMap@/@singleton@ laws:

For any 'Monoid' @m@ and any function @f :: c -> m@,

1. @'foldMap' f@ is a monoid morphism:

    * @'foldMap' f 'mempty' = 'mempty'@
    * @'foldMap' f (m '<>' n) = 'foldMap' f m <> 'foldMap' f n@

2. 'foldMap' undoes 'singleton':

    @'foldMap' f . 'singleton' = f@

Observation laws:

> viewl (singleton e >< s) == e :< s
> viewl empty == EmptyL

The behaviour of '<|','|>', and 'viewr' is implied by the above laws and their
default definitions.
-}
class Traversable s => Sequence s where

  {-# MINIMAL
    empty,
    singleton,
    (viewl | viewr),
    ((><) | (|>) | (<|))
    #-}

  empty     :: s c 
  singleton :: c  -> s c 
  -- | Append two sequences
  (><)       :: s c  -> s c   -> s c 
  -- | View a sequence from the left
  viewl     :: s c  -> ViewL s c 
  -- | View a sequence from the right
  --          
  -- Default definition:
  --
  -- > viewr q = case viewl q of 
  -- >    EmptyL -> EmptyR
  -- >    h :< t -> case viewr t of
  -- >        EmptyR -> empty   :> h
  -- >        p :> l   -> (h <| p) :> l
  --
  viewr     :: s c -> ViewR s c 
  -- | Append a single element to the right
  -- 
  -- Default definition:
  --
  -- > l |> r = l >< singleton r
  -- 
  (|>)       :: s c -> c  -> s c 
  -- | Append a single element to the left
  -- 
  -- Default definition:
  --
  -- > l <| r = singleton l >< r
  --
  (<|)       :: c  -> s c -> s c
  
  -- | Convert a list to a sequence
  --
  -- Default definition:
  --
  -- > fromList = foldl' (|>) empty
  fromList :: [c] -> s c

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

  fromList = foldl' (|>) empty

-- | A view of the left end of a 'Sequence'.
data ViewL s c where
   EmptyL  :: ViewL s c 
   (:<)    :: c -> s c -> ViewL s c

deriving instance (Show c, Show (s c)) => Show (ViewL s c)

-- | A view of the right end of a 'Sequence'.
data ViewR s c where
   EmptyR  :: ViewR s c 
   (:>)    :: s c -> c -> ViewR s c

deriving instance (Show c, Show (s c)) => Show (ViewR s c)

 
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
 fromList = S.fromList

instance Sequence [] where
  empty = []
  singleton x = [x]
  (<|) = (:)
  viewl [] = EmptyL
  viewl (h : t) = h :< t 
  fromList = id
