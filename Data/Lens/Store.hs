{-# LANGUAGE TypeOperators #-}
module Data.Lens.Store
  ( (:->)(..)
  ) where

import Control.Applicative
import Control.Comonad.Trans.Store
import Control.Category
import Data.Functor.Identity
import Data.Functor.Apply
import Data.Semigroupoid
import Prelude hiding ((.), id)
import Data.IntMap (IntMap)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.IntMap as IntMap
import Data.Map (Map)
import qualified Data.Set as Set
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

-- * A reference implementation

newtype a :-> b = Lens { unlens :: a -> Store b a } 

instance Semigroupoid (:->) where
  f `o` g = Lens $ \a -> runLens g a $ \b ba -> runLens f b $ \c cb -> store (ba . cb) c
  {-# INLINE o #-}

instance Category (:->) where
  id = Lens $ store id
  f . g = Lens $ \a -> runLens g a $ \b ba -> runLens f b $ \c cb -> store (ba . cb) c
  {-# INLINE (.) #-}

instance Lens (:->) where
  runLens (Lens f) a k = case f a of StoreT (Identity ba) b -> k b ba
  {-# INLINE runLens #-}
  lens get set = Lens $ \a -> store (\b -> set b a) (get a)
  {-# INLINE lens #-}
  iso f g = Lens $ \a -> store g (f a)
  {-# INLINE iso #-}
