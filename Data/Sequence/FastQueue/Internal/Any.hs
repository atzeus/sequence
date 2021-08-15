{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Trustworthy #-}
#endif
-- We suppress this warning because otherwise GHC complains
-- about the newtype constructor not being used.
#if __GLASGOW_HASKELL__ >= 800
-- This warning doesn't seem to exist before 8.0, by any name.
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
#endif


-- | It's safe to coerce /to/ 'Any' as long as you don't
-- coerce back. We define our own 'Any' instead of using
-- the one in "GHC.Exts" directly to ensure that this
-- module doesn't clash with one making the opposite
-- assumption. We use a newtype rather than a closed type
-- family with no instances because the latter weren't supported
-- until 8.0.
module Data.Sequence.FastQueue.Internal.Any
  ( Any
  , toAny
  , toAnyList
  ) where

import Unsafe.Coerce
import qualified GHC.Exts as E

newtype Any = Any E.Any

-- | Convert anything to 'Any'.
toAny :: a -> Any
toAny = unsafeCoerce

-- | Convert a list of anything to a list of 'Any'.
toAnyList :: [a] -> [Any]
toAnyList = unsafeCoerce
