{-# LANGUAGE CPP #-}
module Data.Lens.Common
  ( 
  -- * Common lenses
    fstLens
  , sndLens
  , mapLens
  , intMapLens
  , setLens
  , intSetLens
  , unitLens
  ) where

import Data.IntMap (IntMap)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.IntMap as IntMap
import Data.Map (Map)
import qualified Data.Set as Set
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Lens.Class

fstLens :: Lens l => l (a,b) a
fstLens = lens fst (\a p -> (a,snd p))
{-# INLINE fstLens #-}

sndLens :: Lens l => l (a,b) b
sndLens = lens snd (\b p -> (fst p, b))
{-# INLINE sndLens #-}

mapLens    :: (Lens l, Ord k) => k -> l (Map k v) (Maybe v)
mapLens k = lens (Map.lookup k) $ \mv m -> case mv of
  Nothing -> Map.delete k m
  Just v' -> Map.insert k v' m
{-# INLINE mapLens #-}

intMapLens :: Lens l => Int -> l (IntMap v) (Maybe v)
intMapLens k = lens (IntMap.lookup k) $ \mv m -> case mv of
  Nothing -> IntMap.delete k m
  Just v' -> IntMap.insert k v' m
{-# INLINE intMapLens #-}

setLens    :: (Lens l, Ord k) => k -> l (Set k) Bool
setLens k = lens (Set.member k) $ \mv m -> if mv 
  then Set.delete k m 
  else Set.insert k m
{-# INLINE setLens #-}

intSetLens :: Lens l =>  Int -> l IntSet Bool
intSetLens k = lens (IntSet.member k) $ \mv m -> if mv 
  then IntSet.delete k m 
  else IntSet.insert k m
{-# INLINE intSetLens #-}

unitLens :: Lens l => l a ()
unitLens = composite (\a -> ((),\() -> a))
{-# INLINE unitLens #-}
