{-# language BangPatterns #-}
{-# language LambdaCase #-}
{-# options_ghc -fno-warn-orphans #-}

module ToCatQueue where
import Data.SequenceClass
import Data.Sequence.ToCatQueue.Internal
import Test.QuickCheck
import Data.List (replicate)
import Data.Foldable (all)
import Valid

{-
data ToCatQueue q a where
  C0 :: ToCatQueue q a
  CN :: a -> !(q (ToCatQueue q a)) -> ToCatQueue q a
  -}

instance Sequence q => Valid (ToCatQueue q) where
  valid C0 = property True
  valid (CN _ q0) = counterexample "Empty queue in simple queue" $ valid' q0
    where
      valid' = all $ \case
        C0 -> False
        CN _ q -> valid' q

instance (Sequence q, Arbitrary a) => Arbitrary (ToCatQueue q a) where
  arbitrary = liftArbitrary arbitrary

-- We are testing only ToCatQueue here, and assume the underlying
-- queue works correctly. So we don't try to generate arbitrary
-- shapes of the underlying queue; we just build those from lists.
instance Sequence q => Arbitrary1 (ToCatQueue q) where
  liftArbitrary el = sized $ \s -> do
    n <- chooseInt (0, s)
    mkArb el n
    
mkArb :: Sequence q => Gen a -> Int -> Gen (ToCatQueue q a)
mkArb _ 0 = pure C0
mkArb g n = do
  x <- g  -- Pick the first element
  sizes <- splat (n - 1) -- Decide how to arrange the remaining ones
  xs <- fromList <$> traverse (mkArb g) sizes
  pure (CN x xs)

  --  rear <- sworp <$> vectorOf rear_len el

-- Generate a partition of a non-negative integer into
-- positive integers. This is not statistically fair; it
-- might be nice to make it so.
splat :: Int -> Gen [Int]
splat n | n < 0 = error "Can't splat a negative number"
splat 0 = pure []
splat n = do
  k <- chooseInt (1, n)
  rest <- splat (n - k)
  pure (k : rest)
