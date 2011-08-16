{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Lens.Combinators
-- Copyright   :  (C) 2011 Edward Kmett,
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- A number of combinators for manipulating lenses
-- 
-- The names are chosen to resemble those in the state monad API
-- with an additional '^' prefix. '!' is used to indicate strictness. 
----------------------------------------------------------------------------
module Data.Lens.Combinators
  ( 
  -- * Lazy Combinators
  -- ** Operator API
    (^$)   -- getter -- :: Lens a b -> a -> b
  , (^.)   -- getter -- :: a -> Lens a b -> b
  , (^=)   -- setter -- :: Lens a b -> b -> (a -> a)
  , (^%=)  -- modify -- :: Lens a b -> (b -> b) -> (a -> a) 
  , (^%%=) -- modify -- :: Functor f => Lens a b -> (b -> f b) -> a -> f a
  -- ** Functional Imperatives
  , (^+=) -- addition
  , (^-=) -- subtraction
  , (^*=) -- multiplication
  , (^/=) -- division

  -- * Strict Combinators
  -- ** Strict Operator API
  , (^$!)   -- getter -- :: Lens a b -> a -> b
  , (^!)    -- getter -- :: a -> Lens a b -> b
  , (^!=)   -- setter -- :: Lens a b -> b -> (a -> a)
  , (^!%=)  -- modify -- :: Lens a b -> (b -> b) -> (a -> a) 
  , (^!%%=) -- modify -- :: Functor f => Lens a b -> (b -> f b) -> a -> f a
  -- ** Strict Functional Imperatives
  , (^!+=) -- addition
  , (^!-=) -- subtraction
  , (^!*=) -- multiplication
  , (^!/=) -- division
  ) where

import Control.Applicative
import Data.Lens.Class
import Data.Lens.Util

infixr 9 ^.
infixr 4 ^=, ^%=, ^%%=, ^/=, ^+=, ^-=, ^*=
infixr 0 ^$

infixr 9 ^!
infixr 4 ^!=, ^!%=, ^!%%=, ^!/=, ^!+=, ^!-=, ^!*=
infixr 0 ^$!

-- | Operator to get the value viewed by the lens. Analogous to ($) for functions.
(^$) :: Lens l => l a b -> a -> b
f ^$ a  = runLens f a const
{-# INLINE (^$) #-}

-- | @flip getL@, used like a field accessor. 
(^.) :: Lens l => a -> l a b -> b
a ^. f = f ^$ a
{-# INLINE (^.) #-}

-- | Functional setter
(^=) :: Lens l => l a b -> b -> a -> a
(^=) f b a = runLens f a $ \ _ g -> g b
{-# INLINE (^=) #-}

-- | Functional modify
(^%=) :: Lens l => l a b -> (b -> b) -> a -> a
(^%=) f g a = runLens f a $ \ b h -> h (g b)
{-# INLINE (^%=) #-}

-- | functorial modify
(^%%=) :: (Functor f, Lens l) => l a b -> (b -> f b) -> a -> f a
(^%%=) f g a = runLens f a $ \b h -> h <$> g b
{-# INLINE (^%%=) #-}

(^+=) :: (Lens l, Num b) => l a b -> b -> a -> a
l ^+= n = l ^%= (+ n)
{-# INLINE (^+=) #-}

(^-=) :: (Lens l, Num b) => l a b -> b -> a -> a
l ^-= n = l ^%= subtract n
{-# INLINE (^-=) #-}

(^*=) :: (Lens l, Num b) => l a b -> b -> a -> a
l ^*= n = l ^%= (* n)
{-# INLINE (^*=) #-}

(^/=) :: (Lens l, Fractional b) => l a b -> b -> a -> a
l ^/= r = l ^%= (/ r)
{-# INLINE (^/=) #-}

-- | Operator to get the value viewed by the lens. Analogous to ($) for functions.
(^$!) :: Lens l => l a b -> a -> b
f ^$! a  = runLens f a const
{-# INLINE (^$!) #-}

-- | @flip getL@, used like a field accessor. 
(^!) :: Lens l => a -> l a b -> b
a ^! f = f ^$! a
{-# INLINE (^!) #-}

-- | Functional setter
(^!=) :: Lens l => l a b -> b -> a -> a
(^!=) f b a = runLens f a $ \ _ g -> g $! b
{-# INLINE (^!=) #-}

-- | Functional modify
(^!%=) :: Lens l => l a b -> (b -> b) -> a -> a
(^!%=) f g a = runLens f a $ \ b h -> h $! g b
{-# INLINE (^!%=) #-}

-- | functorial modify
(^!%%=) :: (Monad f, Lens l) => l a b -> (b -> f b) -> a -> f a
(^!%%=) f g a = runLens f a $ \b h -> h <$!> g b
{-# INLINE (^!%%=) #-}

(^!+=) :: (Lens l, Num b) => l a b -> b -> a -> a
l ^!+= n = l ^%= (+ n)
{-# INLINE (^!+=) #-}

(^!-=) :: (Lens l, Num b) => l a b -> b -> a -> a
l ^!-= n = l ^%= subtract n
{-# INLINE (^!-=) #-}

(^!*=) :: (Lens l, Num b) => l a b -> b -> a -> a
l ^!*= n = l ^%= (* n)
{-# INLINE (^!*=) #-}

(^!/=) :: (Lens l, Fractional b) => l a b -> b -> a -> a
l ^!/= r = l ^%= (/ r)
{-# INLINE (^!/=) #-}
