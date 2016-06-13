{- Build a threadsafe Map with
 - MVars and Map from the standard library.
 -}
module BinaryTreeMap (
  new,
  BinaryTreeMap.lookup,
  BinaryTreeMap,
  contains,
  add,
  remove,
  eval
) where


import Control.Concurrent.MVar
import Control.Exception.Base
import qualified Data.Map.Strict as Map


type BinaryTreeMap k v = MVar (Map.Map k v)

new :: IO (BinaryTreeMap k v)
new = newMVar $ Map.empty

lookup :: Ord k => BinaryTreeMap k v -> k -> IO (Maybe v)
lookup sm k = fmap (Map.lookup k) $ readMVar sm

contains :: Ord k => BinaryTreeMap k v ->  k -> IO Bool
contains sm k = fmap (Map.member k) $ readMVar sm

performOnMap :: (Map.Map k v -> Map.Map k v) -> BinaryTreeMap k v -> IO ()
performOnMap f sm =
  do m <- takeMVar sm
     putMVar sm $ f m

add :: Ord k => BinaryTreeMap k v -> k -> v -> IO ()
add sm k v = performOnMap (Map.insert k v) sm

remove :: Ord k => BinaryTreeMap k v -> k -> IO ()
remove sm k = performOnMap (Map.delete k) sm

eval :: BinaryTreeMap k v -> IO (Map.Map k v)
eval sm = do m <- readMVar sm
             return $! m
