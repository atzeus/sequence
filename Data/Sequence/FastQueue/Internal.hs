{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE BangPatterns #-}


-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Sequence.FastQueue
-- Copyright   :  (c) Atze van der Ploeg 2014
--                (c) David Feuer 2021
-- License     :  BSD-style
-- Maintainer  :  atzeus@gmail.org
-- Stability   :  provisional
-- Portability :  portable
--
-- A queue (actually an output-restricted deque), with worst case constant time:
-- '|>', '<|', and 'viewl'. It has worst case linear time 'viewr'. '><' is linear
-- in the length of its second argument.
--
-- Based on: "Simple and Efficient Purely Functional Queues and Deques", Chris Okasaki,
-- Journal of Functional Programming 1995
--
-----------------------------------------------------------------------------

module Data.Sequence.FastQueue.Internal
  ( FastQueue (..)
  , SL (..)
  , appendSL
  , queue
  ) where
import Data.SequenceClass hiding ((:>))
import qualified Data.SequenceClass as SC
import Data.Foldable
import qualified Data.Traversable as T
import Data.Sequence.FastQueue.Internal.Any
import qualified Control.Applicative as A
import Data.Function (on)
import qualified Text.Read as TR
#if MIN_VERSION_base(4,9,0)
import Data.Functor.Classes (Show1 (..))
import qualified Data.Semigroup as Semigroup
#endif

#if !MIN_VERSION_base(4,8,0)
import Data.Functor (Functor (..))
import Data.Monoid (Monoid (..))
#endif

infixl 5 :>
-- | A strict-spined snoc-list
data SL a
  = SNil
  | !(SL a) :> a
  deriving Functor

-- | Append a snoc list to a list.
appendSL :: [a] -> SL a -> [a]
appendSL l r = rotate l r []
-- precondition : |a| = |f| - (|r| - 1)
-- postcondition: |a| = |f| - |r|
rotate :: [a] -> SL a -> [a] -> [a]
rotate [] (SNil :> y) r = y : r
rotate (x : f) (r :> y) a = x : rotate f r (y : a)
rotate _f _a _r  = error "Invariant |a| = |f| - (|r| - 1) broken"

-- | A scheduled Banker's FastQueue, as described by Okasaki.
data FastQueue a = RQ ![a] !(SL a) ![Any]
  deriving Functor
  -- We use 'Any' rather than an existential to allow GHC to unpack
  -- queues if it's so inclined.

queue :: [a] -> SL a -> [Any] -> FastQueue a
queue f r [] =
  let
    f' = appendSL f r
    {-# NOINLINE f' #-}
  in RQ f' SNil (toAnyList f')
queue f r (_h : t) = RQ f r t

instance Sequence FastQueue where
  empty = RQ [] SNil []
  singleton x =
    let
      c = [x]
      {-# NOINLINE c #-}
    in RQ c SNil (toAnyList c)
  RQ f r a |> x = queue f (r :> x) a

  -- We need to extend the schedule to maintain the
  -- data structure invariant.
  x <| RQ f r a = RQ (x : f) r (toAny () : a)

  (><) = foldl' (|>)

  viewl (RQ [] ~SNil ~[]) = EmptyL
  viewl (RQ (h : t) f a) = h :< queue t f a

  -- Sometimes we get lucky and we can snatch the last element
  -- for free. Sometimes we don't, and it costs us O(n) time.
  viewr (RQ f (rs :> r) a) = RQ f rs (toAny () : a) SC.:> r
  viewr (RQ f SNil _) = case viewr f of
    EmptyR -> EmptyR
    f' SC.:> x -> fromList f' SC.:> x

  fromList xs = RQ xs SNil (toAnyList xs)

instance Show a => Show (FastQueue a) where
    showsPrec p xs = showParen (p > 10) $
        showString "fromList " . shows (toList xs)

#if MIN_VERSION_base(4,9,0)
instance Show1 FastQueue where
  liftShowsPrec _shwsPrc shwList p xs = showParen (p > 10) $
        showString "fromList " . shwList (toList xs)
#endif

instance Read a => Read (FastQueue a) where
    readPrec = TR.parens $ TR.prec 10 $ do
        TR.Ident "fromList" <- TR.lexP
        xs <- TR.readPrec
        return (fromList xs)

    readListPrec = TR.readListPrecDefault

instance Eq a => Eq (FastQueue a) where
  (==) = (==) `on` toList

instance Ord a => Ord (FastQueue a) where
  compare = compare `on` toList

-- -----------------
-- Note: folding and traversing
--
-- We define the Foldable and Traversable instances for this type manually
-- rather than deriving them. This is necessary to maintain the *worst case*
-- performance bounds expected for this type. For example, suppose we convert a
-- FastQueue to a list using toList. Then we expect to be able to consume each
-- cons of the resulting list in O(1) time. If we used the derived instance,
-- and had RQ f r a, then once f was exhausted we'd have to pause to reverse r.
-- Note that `traverse` is inherently a bit weird from a performance
-- standpoint, because it delays building the result structure until the end.
-- There's nothing we can do about this; the Applicative constraint on traverse
-- isn't sufficient to build as we go.

instance Foldable FastQueue where
  -- See note: folding and traversing
  foldr c n = \q -> go q
    where
      go q = case viewl q of
        EmptyL -> n
        h :< t -> c h (go t)
#if MIN_VERSION_base(4,6,0)
  foldl' f b0 = \q -> go q b0
    where
      go q !b = case viewl q of
        EmptyL -> b
        h :< t -> go t (f b h)
#endif

#if MIN_VERSION_base(4,8,0)
  null (RQ [] _ _) = True
  null _ = False
#endif

instance T.Traversable FastQueue where
  -- See note: folding and traversing
  traverse f = fmap fromList . go
    where
      go q = case viewl q of
        EmptyL -> A.pure empty
        h :< t  -> A.liftA2 (:) (f h) (go t)

#if MIN_VERSION_base(4,9,0)
instance Semigroup.Semigroup (FastQueue a) where
  (<>) = (><)
#endif
instance Monoid (FastQueue a) where
  mempty = empty
#if MIN_VERSION_base(4,9,0)
  mappend = (Semigroup.<>)
#else
  mappend = (><)
#endif
