module Data.Lens.Class.Internal
  ( Lens(..)
  ) where

import Control.Category
import Prelude hiding ((.), id)

class Category l => Lens l where
  runLens :: l a b -> a -> (b -> (b -> a) -> r) -> r
  lens :: (a -> b) -> (b -> a -> a) -> l a b

  iso :: (a -> b) -> (b -> a) -> l a b
  iso f g = lens f (\b _ -> g b)

  composite :: (a -> (b, b -> a)) -> l a b
  composite f = lens (fst . f) (\b a -> snd (f a) b)

#ifdef LANGUAGE_Rank2Types
  lensK :: (forall r. a -> (b -> (b -> a) -> r) -> r) -> l a b
  lensK f = composite $ \a -> f a (,)
#endif

#ifdef LANGUAGE_UnboxedTuples
  unboxedLens :: (a -> (# b, b -> a #)) -> l a b
  unboxedLens f = lensK $ \a k -> case f a of (# b, h #) -> k b h
#endif
