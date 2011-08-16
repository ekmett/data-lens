module Data.Lens.Util 
  ( (<$!>) 
  ) where

-- an unexported helper function for strict monadic traversal
infixl 4 <$!>
(<$!>) :: Monad m => (a -> b) -> m a -> m b
f <$!> ma = do
  a <- ma
  return $! f a
