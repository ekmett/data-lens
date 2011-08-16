{-# LANGUAGE CPP, TypeOperators #-}
module Data.Lens.CPS
  ( (:->)(..)
  ) where

import Control.Category
import Prelude hiding ((.), id)
import Data.Lens.Class.Internal

newtype a :-> b = Lens { _runLens :: forall r. (a -> (b -> (b -> a) -> r) -> r) }

instance Category (:->) where
  id = Lens $ \a k -> k a id
  f . g = Lens $ \a k -> runLens g a $ \b ba -> runLens f b $ \c cb -> k c (ba . cb)
  {-# INLINE (.) #-}

instance Lens (:->) where
  runLens = _runLens
  {-# INLINE runLens #-}
  lens get set = Lens $ \a k -> k (get a) (\b -> set b a)
  {-# INLINE lens #-}
  iso f g = Lens $ \a k -> k (f a) g
  {-# INLINE iso #-}
  composite f = Lens $ \a k -> case f a of 
    (b, h) -> k b h
  lensK = Lens
#ifdef LANGUAGE_UnboxedTuples
  unboxedLens f = Lens $ \a k -> case f a of (# b, h #) -> k b h
#endif
