module Valid (Valid (..)) where

import Test.QuickCheck

class Valid s where
  -- Check the invariants of a sequence.
  valid :: s a -> Property
