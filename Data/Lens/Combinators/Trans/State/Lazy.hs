module Data.Lens.Combinators.Trans.State.Lazy
  ( module Data.Lens.Class
  -- * State API
  , access         -- getter -- :: Monad m => Lens a b -> StateT a m b
  , focus          -- modify -- :: Monad m => Lens a b -> StateT m b c -> StateT m a c
  , (~=), (!~=)    -- setter -- :: Monad m => Lens a b -> b -> StateT a m b
  , (%=), (!%=)    -- modify -- :: Monad m => Lens a b -> (b -> b) -> StateT a m b
  , (%%=), (!%%=)  -- modify -- :: Monad m => Lens a b -> (b -> (c, b)) -> StateT a m c
  -- * Functional imperatives
  , (+=), (!+=)    -- modify -- :: (Monad m, Num b) => Lens a b -> b -> StateT a m b
  , (-=), (!-=)    -- modify -- :: (Monad m, Num b) => Lens a b -> b -> StateT a m b
  , (*=), (!*=)    -- modify -- :: (Monad m, Num b) => Lens a b -> b -> StateT a m b
  , (//=), (!/=)   -- modify -- :: (Monad m, Fractional b) => Lens a b -> b -> StateT a m b
  , (&&=), (!&&=)  -- modify -- :: Monad m => Lens a Bool -> Bool -> StateT a m Bool
  , (||=), (!||=)  -- modify -- :: Monad m => Lens a Bool -> Bool -> StateT a m Bool
  ) where

import Control.Monad.Trans.State
import Control.Monad (liftM)
import Data.Lens.Class

infixr 4 ~=, !~=, %=, !%=, %%=, !%%=, +=, !+=, -=, !-=, *=, !*=, //=, !/=, &&=, !&&=, ||=, !||=

-- * State actions

-- | get the value of a lens into state
access :: (Monad m, Lens l) => l a b -> StateT a m b
access f = gets (getL f)
{-# INLINE access #-}

focus :: (Monad m, Lens l) => l a b -> StateT b m c -> StateT a m c
focus f (StateT g) = StateT $ \a -> runLens f a $ \ b h -> liftM (\(c, b') -> (c, h b')) (g b)
{-# INLINE focus #-}

-- | set a value using a lens into state
(~=) :: (Monad m, Lens l) => l a b -> b -> StateT a m b
f ~= b = StateT $ \a -> runLens f a $ \ _ k -> return (b, k b)
{-# INLINE (~=) #-}

-- | strict setter 
(!~=) :: (Monad m, Lens l) => l a b -> b -> StateT a m b
f !~= b = StateT $ \a -> runLens f a $ \_ k -> return $! (b, k $! b)
{-# INLINE (!~=) #-}

    
-- | infix modification a value through a lens into state
(%=), (!%=) :: (Monad m, Lens l) => l a b -> (b -> b) -> StateT a m b
f %= g = StateT $ \a -> runLens f a $ \b h -> let b' = g b in return (b', h b') 
f !%= g = StateT $ \a -> runLens f a $ \b h -> let b' = g b in return $! (b', h $! b') 
{-# INLINE (%=) #-}
{-# INLINE (!%=) #-}


-- | infix modification of a value through a lens into state
-- with a supplemental response
(%%=), (!%%=) :: (Monad m, Lens l) => l a b -> (b -> (c, b)) -> StateT a m c
f %%= g = StateT $ \a -> runLens f a $ \b h -> let (c,b') = g b in return (c, h b')
f !%%= g = StateT $ \a -> runLens f a $ \b h -> let (c,b') = g b in return $! (c, h $! b')
{-# INLINE (%%=) #-}
{-# INLINE (!%%=) #-}

(+=), (!+=), (-=), (!-=), (*=), (!*=) :: (Monad m, Num b, Lens l) => l a b -> b -> StateT a m b
f += b = f %= (+ b)
f -= b = f %= subtract b
f *= b = f %= (* b)
f !+= b = f !%= (+ b)
f !-= b = f !%= subtract b
f !*= b = f !%= (* b)
{-# INLINE (+=) #-}
{-# INLINE (-=) #-}
{-# INLINE (*=) #-}
{-# INLINE (!+=) #-}
{-# INLINE (!-=) #-}
{-# INLINE (!*=) #-}

(//=), (!/=) :: (Monad m, Fractional b, Lens l) => l a b -> b -> StateT a m b
f //= b = f %= (/ b)
f !/= b = f !%= (/ b)
{-# INLINE (//=) #-}
{-# INLINE (!/=) #-}

(&&=), (||=), (!&&=), (!||=) :: (Monad m, Lens l) => l a Bool -> Bool -> StateT a m Bool
f &&= b = f %= (&& b)
f ||= b = f %= (|| b)
f !&&= b = f !%= (&& b)
f !||= b = f !%= (|| b)
{-# INLINE (&&=) #-}
{-# INLINE (||=) #-}
{-# INLINE (!&&=) #-}
{-# INLINE (!||=) #-}
