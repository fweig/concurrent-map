module ConcurrentMap
    ( ConcurrentMap
    , new
    , new'
    , add
    , remove
    , contains
    , ConcurrentMap.lookup
    ) where

import qualified Skiplist

data ConcurrentMap k v = ConcurrentMap (Skiplist.Skiplist k v)

{- 
 - Create a new concurrent map.
 -}
new :: IO (ConcurrentMap k v)
new = new' 30 0.5

{-
 - Create a new concurrent map and
 - set the parameters for the underlying
 - skiplist.
 -}
new' :: Int -> Double -> IO (ConcurrentMap k v)
new' maxHeight pNewLayer = 
    do sl <- Skiplist.new maxHeight pNewLayer
       return $ ConcurrentMap sl

{-
 - Add a key-value pair to the map.
 - Returns False if the key can already be
 - found.
 -}
add :: Ord k => ConcurrentMap k v -> k -> v -> IO Bool
add (ConcurrentMap sl) = Skiplist.add sl

{-
 - Remove a key from the map.
 - Return False if the key could not be found.
 -}
remove :: Ord k => ConcurrentMap k v -> k -> IO Bool
remove (ConcurrentMap sl) = Skiplist.remove sl

{-
 - Check whether a key is in the map.
 -}
contains :: Ord k => ConcurrentMap k v -> k -> IO Bool
contains (ConcurrentMap sl) = Skiplist.contains sl

{-
 - Returns the value associated with the provided key.
 - Will return Nothing if the key could not be found.
 -}
lookup :: Ord k => ConcurrentMap k v -> k -> IO (Maybe v)
lookup (ConcurrentMap sl) = Skiplist.lookup sl
