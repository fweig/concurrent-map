module Skiplist (
    new,
    add,
    remove,
    contains,
    layersToStr,
    Skiplist.lookup,
    verify,
    Skiplist,
) where


import GuardedValue

import Control.Concurrent 
import Control.Monad

import Data.Array
import Data.IORef
import Data.List
import Data.Maybe

import System.Random


data SkiplistNode k v = SkiplistNode { key              :: GuardedValue k
                                     , value            :: Maybe v
                                     , markedForRemoval :: IORef Bool
                                     , fullyLinked      :: MVar ()
                                     , lock             :: MVar ()
                                     , succs            :: Array Int (IORef (SkiplistNode k v)) }

data Skiplist k v = Skiplist { maxHeight :: Int
                             , pNewLayer :: Double
                             , first     :: SkiplistNode k v }


type FindNodeCollect k v = [( SkiplistNode k v
                             , SkiplistNode k v
                             , Int )] 


instance (Show k, Show v) => Show (SkiplistNode k v) where
    show node = (show $ key node) ++ "\t" ++ (showHeight $ succs node)

{-
 - Simulate pointer-equality on nodes, by comparing theirs locks.
 - This works as every nodes holds a unique lock.
 -}
instance Eq (SkiplistNode k v) where
    x == y = (lock x) == (lock y)


{-
 - Create a new node for the skiplist.
 -}
newSkiplistNode :: GuardedValue k -> Maybe v -> [SkiplistNode k v] -> IO (SkiplistNode k v)
newSkiplistNode key value succs = 
    do l <- newMVar ()
       succs' <- forM succs newIORef
       marked <- newIORef False
       linked <- newEmptyMVar
       return $ SkiplistNode key value marked linked l $ listArray (0, length succs'-1) succs'

{-
 - Check whether a node was deleted.
 -}
isMarked :: SkiplistNode k v -> IO Bool
isMarked = readIORef . markedForRemoval

{- 
 - Mark a node as deleted.
 -}
mark :: SkiplistNode k v -> IO ()
mark node = atomicWriteIORef (markedForRemoval node) True

waitUntilFullyLinked :: SkiplistNode k v -> IO ()
waitUntilFullyLinked = readMVar . fullyLinked

{- 
 - Mark the node as fully linked.
 -}
setFullyLinked :: SkiplistNode k v -> IO ()
setFullyLinked node = putMVar (fullyLinked node) ()

isFullyLinked :: SkiplistNode k v -> IO Bool
isFullyLinked node = fmap not $ isEmptyMVar (fullyLinked node)

isLast :: SkiplistNode k v -> Bool
isLast node = null $ succs node

getSuccRef :: SkiplistNode k v -> Int -> IORef (SkiplistNode k v)
getSuccRef node layer = (succs node) ! layer

getSucc :: SkiplistNode k v -> Int -> IO (SkiplistNode k v)
getSucc node layer = readIORef $ getSuccRef node layer

setSucc :: SkiplistNode k v -> Int -> SkiplistNode k v -> IO ()
setSucc node layer newSucc = atomicWriteIORef (getSuccRef node layer) newSucc

getHeight :: SkiplistNode k v -> Int
getHeight =  max 1 . length . succs

isInList :: SkiplistNode k v -> IO Bool
isInList node =
    do marked <- isMarked node
       fullyLinked <- isFullyLinked node
       return (not marked && fullyLinked)

showHeight xs = replicate (length xs) 'x'

upmostLayer :: Skiplist k v -> Int
upmostLayer sl = (maxHeight sl) - 1

{-
 - Prevent another thread from editing this node.
 -}
block :: SkiplistNode k v -> IO ()
block = takeMVar . lock

{-
 - Free the node for other threads.
 -}
unblock :: SkiplistNode k v -> IO ()
unblock node = putMVar (lock node) ()

getLayer ::(Ord k) => SkiplistNode k v -> Int -> IO [SkiplistNode k v]
getLayer curr layer | isLast curr = return [curr]
                    | otherwise    = do succ <- getSucc curr layer
                                        nextNodes <- getLayer succ layer
                                        return $ curr:nextNodes

{-
 - Debug function.
 - Show the individual layers of the skiplist.
 -}
layersToStr :: (Ord k, Show k) => Skiplist k v -> IO String
layersToStr sl =
    do layers <- forM [0..upmostLayer sl] $ getLayer (first sl)
       let layerToStr l = concat $ intersperse " " $ map (show . key) l
           layers' = map layerToStr layers
       return $ concat $ intersperse "\n" $ reverse layers'

verifyLayer :: (Ord k) => SkiplistNode k v -> Int -> IO Bool
verifyLayer curr layer | isLast curr = return True
                       | otherwise   = do succ <- getSucc curr layer
                                          if key succ > key curr
                                          then verifyLayer succ layer
                                          else return False

{-
 - Debug function.
 - Checks whether the skiplist structure is valid and
 - if it contains a list of provided keys.
 -}
verify :: (Show k, Ord k) => Skiplist k v -> [k] -> IO Bool
verify sl keysInList = 
     do let layerInds = [0..upmostLayer sl]
        keysFound  <- forM keysInList $ contains sl
        if and keysFound
        then do layersOk <- forM layerInds (verifyLayer (first sl))
                return (and layersOk)
        else return False


findPred :: Ord k => SkiplistNode k v
                  -> SkiplistNode k v 
                  -> GuardedValue k 
                  -> Int 
                  -> IO (SkiplistNode k v, SkiplistNode k v)
findPred pred curr searchedKey layer 
    -- Advance to the position where 'searchedKey' has to be inserted.
    | searchedKey > key curr = do succ <- getSucc curr layer
                                  findPred curr succ searchedKey layer
    | otherwise = return (pred, curr)

collectPreds :: (Ord k) => SkiplistNode k v 
                        -> GuardedValue k
                        -> (Maybe (SkiplistNode k v), FindNodeCollect k v) 
                        -> Int
                        -> IO (Maybe (SkiplistNode k v), FindNodeCollect k v)
collectPreds slHead searchedKey (searchedNode, accs) layer =
    do let pred = case accs of []          -> slHead
                               (p, _, _):_ -> p
           searchedNode' curr = 
             if isNothing searchedNode && key curr == searchedKey
             then Just curr
             else searchedNode
       -- Immediately advance to succesor, as 'key pred < searchedKey' is
       -- always true at this point.
       curr <- getSucc pred layer
       (pred', curr') <- findPred pred curr searchedKey layer
       return ( searchedNode' curr'
              , (pred', curr', layer):accs)

{- 
 - Tries to find the node associated with a key.
 - Also collects the predecessor and successor in every layer
 - if the key is being inserted.
 -}
findNode :: Ord k => Skiplist k v 
                  -> GuardedValue k 
                  -> IO (Maybe (SkiplistNode k v), FindNodeCollect k v)
findNode sl k =
    do let maxHeight = upmostLayer sl
           layers = [maxHeight,maxHeight-1..0]
       foldM (collectPreds (first sl) k) (Nothing, []) layers

{-
 - Check whether a key is in the skiplist.
 -}
contains :: Ord k => Skiplist k v -> k -> IO Bool
contains sl k =
    do (searchedKey, _) <- findNode sl $ Value k
       case searchedKey of
         Nothing -> return False
         Just searchedKey' -> isInList searchedKey'

{-
 - Returns the value associated with the provided key.
 - Will return Nothing if the key could not be found.
 -}
lookup :: Ord k => Skiplist k v -> k -> IO (Maybe v)
lookup sl k =
    do (searchedKey, _) <- findNode sl $ Value k
       case searchedKey of
         Nothing -> return Nothing
         Just searchedKey' -> do inList <- isInList searchedKey'
                                 return $ if inList
                                          then value searchedKey'
                                          else Nothing
       
{-
 - Determine the height of a new node.
 -}
randomLevel :: Skiplist k v -> IO Int
randomLevel sl =
  do rGen <- newStdGen
     let lvl = (length $ takeWhile (<=pNewLayer sl) 
                       $ take (maxHeight sl-1) 
                       $ randomRs (0.0, 1.0) rGen) + 1
     return lvl

{- 
 - Create a new skiplist.
 -}
new :: Int -> Double -> IO (Skiplist k v)
new maxHeight pNewLayer =
    do rGuard <- newSkiplistNode RightSentinel Nothing []
       setFullyLinked rGuard
       lGuard <- newSkiplistNode LeftSentinel Nothing (replicate maxHeight rGuard)
       setFullyLinked lGuard
       return Skiplist { maxHeight = maxHeight
                       , pNewLayer = pNewLayer
                       , first     = lGuard }


unlockNodes :: [SkiplistNode k v] -> IO ()
unlockNodes nodes = 
    -- Group nodes to avoid locking duplicates.
    forM_ (group nodes) (\(n:_) -> unblock n)

tryLockNodes :: Ord k => FindNodeCollect k v
                      -> (SkiplistNode k v -> SkiplistNode k v -> Int -> IO Bool)
                      -> IO Bool
tryLockNodes accs isValid =
    let rollback node alreadyLocked = 
            do unless alreadyLocked $ unblock node
               return False
        tryLockNodesAcc _ [] = return True
        tryLockNodesAcc pred ((curr, succ, layer):accs) =
            do let alreadyLocked = pred == Just curr
               unless alreadyLocked $ block curr
               nodesValid <- isValid curr succ layer
               if nodesValid
               then do nodesLocked <- tryLockNodesAcc (Just curr) accs
                       if nodesLocked
                       then return True
                       else rollback curr alreadyLocked
               else rollback curr alreadyLocked
     in tryLockNodesAcc Nothing accs

{-
 - Add a key-value pair to the skiplist.
 - Returns False if the key can already be
 - found.
 -}
add :: Ord k => Skiplist k v -> k -> v -> IO Bool
add sl k v = 
    do let k' = Value k
       (searchedNode, lookupAccs) <- findNode sl k'
       let (preds, succs, layers) = unzip3 lookupAccs
       case searchedNode of
         Just nodeFound -> 
           do marked <- isMarked nodeFound
              if marked
              -- Key was deleted, so just reinsert it.
              then add sl k v
              else do waitUntilFullyLinked nodeFound
                      return False
         Nothing ->
           do h <- randomLevel sl
              let toLock = take h lookupAccs
                  newNodePreds = take h $ preds
                  newNodeSuccs = take h $ succs
              predsLocked <- tryLockNodes toLock isValidAdd 
              if predsLocked
              then do newNode <- newSkiplistNode k' (Just v) newNodeSuccs
                      zipWithM_ (\p l -> setSucc p l newNode) newNodePreds layers 
                      setFullyLinked newNode
                      unlockNodes newNodePreds
                      return True
              else add sl k v
                     
{- 
 - Try to acquire a node and mark it as deleted.
 - Will return False, if the node was deleted by another thread.
 -}
markForRemoval :: SkiplistNode k v -> Bool -> IO Bool
markForRemoval _ True = return True
markForRemoval node _ =
    do block node
       marked <- isMarked node
       if marked
       then do unblock node
               return False
       else do mark node
               return True

isValidAdd :: SkiplistNode k v ->  SkiplistNode k v -> Int -> IO Bool
isValidAdd p s l = 
    do succRem <- isMarked s
       predRem <- isMarked p
       s'      <- getSucc p l
       return $ not (succRem || predRem || s' /= s)

isValidRemove :: SkiplistNode k v -> SkiplistNode k v -> Int -> IO Bool
isValidRemove p s l = do predRem <- isMarked p
                         s'      <- getSucc p l
                         return $ not (predRem || s' /= s)

removeAcc :: Ord k => Skiplist k v -> GuardedValue k -> Bool -> IO Bool
removeAcc sl k isMarked =
    do (searchedKey, lookupAccs) <- findNode sl k
       case searchedKey of
         Just nodeToDelete ->
           do isOkToRemove <- isInList nodeToDelete 
              if isMarked || isOkToRemove
              then do isMarked' <- markForRemoval nodeToDelete isMarked
                      if isMarked'
                      then do let (preds, _, _) = unzip3 lookupAccs
                                  toLock = take (getHeight nodeToDelete) $ lookupAccs
                                  preds' = take (getHeight nodeToDelete) $ preds
                              predsLocked <- tryLockNodes toLock isValidRemove
                              if predsLocked
                              then do zipWithM_ 
                                        (\p l -> setSucc p l =<< getSucc nodeToDelete l) preds' [0..]
                                      unblock nodeToDelete
                                      unlockNodes preds'
                                      return True
                              else removeAcc sl k True
                      else return False     
              else return False
         Nothing -> return False

{-
 - Remove a key from the skiplist.
 - Return False if the key could not be found.
 -}
remove :: Ord k => Skiplist k v -> k -> IO Bool
remove sl k = removeAcc sl (Value k) False
