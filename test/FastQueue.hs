{-# language BangPatterns #-}
module FastQueue where
import Data.Sequence.FastQueue.Internal
import Test.QuickCheck
import Data.Sequence.Any
import Data.List (replicate)
import Valid

instance Valid FastQueue where
  valid (RQ front rear schedule) =
    counterexample "fails |front| = |rear| + |sched|" $
    length front === slen rear + length schedule

instance Arbitrary a => Arbitrary (FastQueue a) where
  arbitrary = liftArbitrary arbitrary

instance Arbitrary1 FastQueue where
  liftArbitrary el = do
    NonNegative rear_len <- arbitrary
    NonNegative schedule_len <- arbitrary
    rear <- sworp <$> vectorOf rear_len el
    front <- vectorOf (schedule_len + rear_len) el
    pure $ RQ front rear (replicate schedule_len (toAny ()))
    where
      sworp [] = SNil
      sworp (x : xs) = sworp xs :> x

slen :: SL a -> Int
slen = go 0
  where
    go !acc SNil = acc
    go acc (xs :> _) = go (acc + 1) xs
