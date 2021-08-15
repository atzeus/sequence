{-# language BangPatterns #-}
{-# options_ghc -fno-warn-orphans #-}
module BSeq where
import Data.Sequence.BSeq.Internal
import Test.QuickCheck
import Valid


instance Valid BSeq where
  valid Empty = property True
  valid x = counterexample "Empty under Node" $ valid' x
    where
      valid' Empty = False
      valid' (Leaf _) = True
      valid' (Node l r) = valid' l && valid' r

instance Arbitrary a => Arbitrary (BSeq a) where
  arbitrary = liftArbitrary arbitrary

instance Arbitrary1 BSeq where
  liftArbitrary el = sized $ \s -> do
    len <- chooseInt (0, s)
    mkArb len el

-- Generate an arbitrary sequence of exactly the given size.
mkArb :: Int -> Gen a -> Gen (BSeq a)
mkArb 0 _ = pure Empty
mkArb 1 g = Leaf <$> g
mkArb n g = do
  fs <- chooseInt (1, n - 1)
  let rs = n - fs
  Node <$> mkArb fs g <*> mkArb rs g
