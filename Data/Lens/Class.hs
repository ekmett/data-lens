module Data.Lens.Class
  ( Lens(runLens, lens, iso, composite)
  , getL
  , setL, setL'
  , modL, modL' 
  , fmodL, fmodL'
  ) where

import Control.Applicative
import Data.Lens.Class.Internal
import Data.Lens.Util

-- | Get a value from the lens.
getL  :: Lens l => l a b -> a -> b
getL l a = runLens l a const
{-# INLINE getL #-}

-- | Set a value through the lens
setL  :: Lens l => l a b -> b -> a -> a
setL l b a = runLens l a $ \ _ h -> h b
{-# INLINE setL #-}

-- | Modify the value viewed by the lens
modL :: Lens l => l a b -> (b -> b) -> a -> a
modL l f a = runLens l a $ \ b h -> h (f b)
{-# INLINE modL #-}

-- | Generate many new values using a lens
fmodL :: (Functor f, Lens l) => l a b -> (b -> f b) -> a -> f a
fmodL l f a = runLens l a $ \ b h -> h <$> f b
{-# INLINE fmodL #-}

-- | Set a value through the lens, forcing it before inserting it
setL'  :: Lens l => l a b -> b -> a -> a
setL' l b a = runLens l a $ \ _ h -> h $! b
{-# INLINE setL' #-}

-- | Modify the value viewed by the lens, forcing it before reinserting
modL' :: Lens l => l a b -> (b -> b) -> a -> a
modL' l f a = runLens l a $ \ b h -> h $! f b
{-# INLINE modL' #-}

-- | Generate many new values using a lens, strictly forcing each element
fmodL' :: (Monad f, Lens l) => l a b -> (b -> f b) -> a -> f a
fmodL' l f a = runLens l a $ \ b h -> h <$!> f b
{-# INLINE fmodL' #-}
