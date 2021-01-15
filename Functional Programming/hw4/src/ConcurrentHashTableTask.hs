{-# LANGUAGE ScopedTypeVariables #-}

-- | This module contains an implementation 
-- of a thread-safe hashtable data structure.
module ConcurrentHashTableTask
  ( ConcurrentHashTable
  , newCHT
  , sizeCHT
  , putCHT
  , getCHT
  ) where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (TVar, modifyTVar, newTVar, readTVar,
                                    writeTVar)
import Control.Monad (when)
import Data.Foldable (forM_)
import Data.Hashable (Hashable, hash)
import Data.Vector (Vector, length, replicateM, toList, (!))

type Bucket k v = TVar [(k, v)]

-- | Data type representing the hashtable.
data ConcurrentHashTable k v = ConcurrentHashTable
  { chtSize    :: TVar Int,                   -- Amount of elements in this map
    chtBuckets :: TVar (Vector (Bucket k v))  -- Map buckets, storing the values
  }

-- | Initial amount of buckets.
initialSize :: Int
initialSize = 16

-- | Max load factor before size increase.
loadFactor :: Float
loadFactor = 0.75

-- | Returns a new empty hashtable.
newCHT :: IO (ConcurrentHashTable k v)
newCHT = atomically $ do
  size <- newTVar 0
  buckets <- replicateM initialSize (newTVar [])
  bucketsTVar <- newTVar buckets
  return
    $! ConcurrentHashTable
      { chtSize = size,
        chtBuckets = bucketsTVar
      }

-- | Returns the amount of elements in the provided hashtable.
sizeCHT :: ConcurrentHashTable k v -> IO Int
sizeCHT c = atomically $ readTVar (chtSize c)

-- | Insert a key-value pair in the provided hashtable.
putCHT :: (Eq k, Hashable k) => k -> v -> ConcurrentHashTable k v -> IO ()
putCHT k v t = atomically $ do
  size <- readTVar (chtSize t)
  oldBuckets <- readTVar (chtBuckets t)
  let bucketsLengthOld = Data.Vector.length oldBuckets
  when (fromIntegral size / fromIntegral bucketsLengthOld > loadFactor) $ resize oldBuckets
  newBuckets <- readTVar (chtBuckets t)
  addToBuckets k v newBuckets
  modifyTVar (chtSize t) (+ 1)
  where
    resize buckets = do
      entries <- fmap concat (mapM readTVar (toList buckets))
      newBuckets <- replicateM (2 * Data.Vector.length buckets) (newTVar [])
      forM_ entries (\(k', v') -> addToBuckets k' v' newBuckets)
      writeTVar (chtBuckets t) newBuckets

    addToBuckets key value buckets = do
      size <- readTVar (chtSize t)
      let bucketTVar = buckets ! (hash key `mod` Data.Vector.length buckets)
      bucket <- readTVar bucketTVar
      let bucket' = filter (\(k', _) -> k' /= key) bucket
      when (Prelude.length bucket /= Prelude.length bucket') $ writeTVar (chtSize t) (size - 1)
      writeTVar bucketTVar ((key, value) : bucket')

-- | Retrieve an value from the hastable by key.
-- 'Just <value>' if key is present, 'Nothing' otherwise
getCHT :: (Eq k, Hashable k) => k -> ConcurrentHashTable k v -> IO (Maybe v)
getCHT k t = atomically $ do
  buckets <- readTVar (chtBuckets t)
  let bucketTVar = buckets ! (hash k `mod` Data.Vector.length buckets)
  bucket <- readTVar bucketTVar
  return (lookup k bucket)
