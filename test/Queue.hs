{-# language BangPatterns #-}
{-# options_ghc -fno-warn-orphans #-}

module Queue where
import Data.Sequence.Queue.Internal
import Test.QuickCheck
import Valid

{-
data P a = a :* a
  deriving (Functor, Foldable, Traversable)

data B a where
  B1 :: a    -> B a
  B2 :: !(P a)  -> B a

data Queue a  where
  Q0 :: Queue a
  Q1 :: a  -> Queue a
  QN :: !(B a) -> Queue (P a) -> !(B a) -> Queue a
  -}


instance Valid Queue where
  -- Just force the structure to make sure it's finite
  -- and fully defined.
  valid = property . foldr (\_ r -> r) True

instance Arbitrary a => Arbitrary (Queue a) where
  arbitrary = liftArbitrary arbitrary

instance Arbitrary1 Queue where
  liftArbitrary el = sized $ \s -> do
    len <- chooseInt (0, s)
    mkArbQ len el

-- Generate an arbitrary queue of exactly the given size.
mkArbQ :: Int -> Gen a -> Gen (Queue a)
mkArbQ 0 _ = pure Q0
mkArbQ 1 g = Q1 <$> g
mkArbQ 2 g = QN <$> mkArbB 1 g <*> pure Q0 <*> mkArbB 1 g
mkArbQ 3 g = oneof
  [ QN <$> mkArbB 1 g <*> pure Q0 <*> mkArbB 2 g
  , QN <$> mkArbB 2 g <*> pure Q0 <*> mkArbB 1 g ]
mkArbQ n g
  | even n
  = oneof
      [ QN <$> mkArbB 2 g <*> mkArbQ ((n - 4) `quot` 2) (mkArbP g) <*> mkArbB 2 g
      , QN <$> mkArbB 1 g <*> mkArbQ ((n - 2) `quot` 2) (mkArbP g) <*> mkArbB 1 g ]
  | otherwise
  = oneof
      [ QN <$> mkArbB 2 g <*> mkArbQ ((n - 3) `quot` 2) (mkArbP g) <*> mkArbB 1 g
      , QN <$> mkArbB 1 g <*> mkArbQ ((n - 3) `quot` 2) (mkArbP g) <*> mkArbB 2 g ]

mkArbB :: Int -> Gen a -> Gen (B a)
mkArbB 1 g = B1 <$> g
mkArbB 2 g = B2 <$> mkArbP g
mkArbB _ _ = error "mkArbB must be called with 1 or 2."

mkArbP :: Gen a -> Gen (P a)
mkArbP g = (:*) <$> g <*> g
