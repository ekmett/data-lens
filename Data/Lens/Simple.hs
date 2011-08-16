{-# LANGUAGE TypeOperators #-}
module Data.Lens.Simple
  ( (:->)(..)
  ) where

import Control.Category
import Prelude hiding ((.), id)
import Data.Lens.Class

-- * A reference implementation

newtype a :-> b = Lens { unlens :: a -> (b, b -> a) } 

instance Category (:->) where
  id = Lens $ \a -> (a, id)
  f . g = Lens $ \a -> runLens g a $ \b ba -> runLens f b $ \c cb -> (c, ba . cb)
  {-# INLINE (.) #-}

instance Lens (:->) where
  runLens (Lens f) a k = case f a of (b, ba) -> k b ba
  {-# INLINE runLens #-}
  lens get set = Lens $ \a -> (get a, \b -> set b a)
  {-# INLINE lens #-}
  iso f g = Lens $ \a -> (f a, g)
  {-# INLINE iso #-}
