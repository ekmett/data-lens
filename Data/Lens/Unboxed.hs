{-# LANGUAGE TypeOperators #-}
module Data.Lens.Unboxed
  ( (:->)(..)
  ) where

import Control.Category
import Prelude hiding ((.), id)
import Data.Lens.Class.Internal

-- * A reference implementation

newtype a :-> b = Lens { unlens :: a -> (# b, b -> a #) } 

instance Category (:->) where
  id = Lens $ \a -> (# a, id #)
  f . g = Lens $ \a -> case unlens g a of
    (# b, ba #) -> case unlens f b of
      (# c, cb #) -> (# c, ba . cb #)
  {-# INLINE (.) #-}

instance Lens (:->) where
  runLens f a k = case unlens f a of (# b, g #) -> k b g
  {-# INLINE runLens #-}
  lens get set = Lens $ \a -> (# get a,  \b -> set b a #)
  {-# INLINE lens #-}
  iso f g = Lens $ \a -> (# f a, g #)
  {-# INLINE iso #-}
  composite f = Lens $ \a -> case f a of (b, h) -> (# b, h #)
  unboxedLens = Lens
