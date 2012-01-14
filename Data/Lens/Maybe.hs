-- | Lenses that project on to a @Maybe@ value.
module Data.Lens.Maybe
  ( MLens
  -- * Functional API
  , mgetL
  , mset
  , unset
  -- * Operator API
  , (^|), (^|!)   -- getter
  , (^|=), (^|!=) -- setter
  ) where

import Data.Lens.Common
import Control.Comonad.Trans.Store
import Data.Maybe

-- | 
type MLens a b =
  Lens a (Maybe b)

-- | Gets the @Just@ value from a lens or a default if @Nothing@.
mgetL ::
  MLens a b
  -> a
  -> b -- ^ The default if @Nothing@.
  -> b
mgetL =
  (^|)

-- | Gets the @Just@ value from a lens or a default if @Nothing@.
(^|) ::
  MLens a b
  -> a
  -> b -- ^ The default if @Nothing@.
  -> b
l ^| a =
  flip fromMaybe (l ^$ a)

-- | Gets the @Just@ value from a lens or a default if @Nothing@.
(^|!) ::
  MLens a b
  -> a
  -> b -- ^ The default if @Nothing@.
  -> b
l ^|! a =
  flip fromMaybe (l ^$! a)

-- | Sets a @Just@ value on a lens.
mset ::
  MLens a b
  -> b -- ^ The value to set.
  -> a
  -> a
mset =
  (^|=)

-- | Sets a @Just@ value on a lens.
(^|=) ::
  MLens a b
  -> b -- ^ The value to set.
  -> a
  -> a
l ^|= b =
  l ^= Just b

-- | Sets a @Just@ value on a lens.
(^|!=) ::
  MLens a b
  -> b -- ^ The value to set.
  -> a
  -> a
l ^|!= b =
  l ^!= Just b

-- | Sets a @Nothing@ value on a lens.
unset ::
  MLens a b
  -> a
  -> a
unset l =
  l ^= Nothing

setJust ::
  MLens a b
  -> Lens b c
  -> a
  -> c
  -> a
setJust (Lens f) g a c =
  let a' = f a
  in case pos a' of
       Nothing -> a
       Just b  -> peek (Just (setL g c b)) a' 

