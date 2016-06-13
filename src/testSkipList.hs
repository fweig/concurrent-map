{-
 - Utility to hunt race conditions in the skiplist and to
 - test the correctness of the implemented algorithm.
 -}
module Main 
  where

import Skiplist
import Fork

import Control.Concurrent
import Control.Monad

import Data.Array.IO

import System.Environment
import System.Exit
import System.Random


type Testbench k v = [Int] -> [Int] -> [Double] -> [Int] 
                     -> [Int] -> IO () 

testValues = [ 878
             , 46865
             , 33392
             , 91871
             , 87196
             , 45839
             , 57947
             , 99244
             , 10913
             , 31819 ]

reportFailure :: String -> IO ()
reportFailure testName = 
    do putStrLn ("[FAILED] " ++ testName)
       exitFailure       

reportSuccess :: String -> IO ()
reportSuccess testName = putStrLn ("[OK] " ++ testName)

checkpoint :: String -> Bool -> IO ()
checkpoint _  True = return ()
checkpoint testname False = reportFailure testname

checkpointVerbose :: String -> Bool -> IO ()
checkpointVerbose testname True  = reportSuccess testname
checkpointVerbose testname False = reportFailure testname

testAdd :: Bool -> [Int] -> Int -> Double -> Int -> Int -> IO ()
testAdd splitValues testValues threadCount pNewLayer height seed = 
    do setStdGen $ mkStdGen seed 
       sl <- new height pNewLayer 
       let testValues' = take (elemsPerThread * threadCount) testValues
           elemsPerThread = (length testValues) `div` threadCount
           threadElems i  = if splitValues
                            then take elemsPerThread $ drop (i * elemsPerThread) testValues'
                            else testValues
       guards <- forM (map threadElems [0..threadCount-1]) 
                      (\xs -> forkIOWait $ do forM_ xs (\x -> add sl x ()))
       joinThreads guards
       skiplistOk <- verify sl testValues'
       checkpoint ("Tested add with " ++ show threadCount ++ " threads and height " ++ show height) 
                  skiplistOk

testRemove :: Bool -> [Int] -> Int -> Double -> Int -> Int -> IO ()
testRemove splitValues testValues threadCount pNewLayer height seed =
    do setStdGen $ mkStdGen seed 
       sl <- new height pNewLayer 
       forM_ testValues $ \k -> add sl k ()
       let testValues' = take (elemsPerThread * threadCount) testValues
           elemsPerThread = (length testValues) `div` threadCount
           threadElems i  = if splitValues
                            then take elemsPerThread $ drop (i * elemsPerThread) testValues'
                            else testValues
       guards <- forM (map threadElems [0..threadCount-1])
                      (\xs -> forkIOWait $ forM_ xs (\x -> remove sl x))
       joinThreads guards
       testValuesInList <- forM testValues'$ contains sl
       layers <- layersToStr sl
       checkpoint ("Remove: " ++ show threadCount ++ " threads and height " ++ show height 
                    ++ ". Failed to delete all keys.\n" ++ layers)
                  (all not testValuesInList)
       skiplistOk <- verify sl []
       checkpoint ("Remove: " ++ show threadCount ++ " threads and height " ++ show height 
                    ++ ". List is broken.")
                  skiplistOk 

shuffle :: [a] -> IO [a]
shuffle xs = 
    let n = length xs
        newArray :: Int -> [a] -> IO (IOArray Int a)
        newArray n xs =  newListArray (1,n) xs
     in do ar <- newArray n xs
           forM [1..n] $ \i -> do j <- randomRIO (i,n)
                                  vi <- readArray ar i
                                  vj <- readArray ar j
                                  writeArray ar j vi
                                  return vj
                  
testAddAndRemove :: [Int] -> Int -> Double -> Int -> Int -> IO ()
testAddAndRemove testValues threadCount pNewLayer height seed =
    do setStdGen $ mkStdGen seed
       sl <- new height pNewLayer 
       let (toDelete, undeleted) = splitAt (length testValues `div` 2) testValues
           actions = [add sl k () | k <- testValues] ++ [remove sl k | k <- toDelete] 
           performActions :: [IO a] -> IO [a]
           performActions [] = return []
           performActions (io:ios) = do r <- io
                                        rs <- performActions ios
                                        return $ r:rs
       guards <- forM [1..threadCount]
                      (\_ -> do actions' <- shuffle actions
                                forkIOWait $ do performActions actions; return ())
       joinThreads guards
       skiplistOk <- verify sl undeleted
       checkpoint ("Remove and add failed.") skiplistOk

testList :: Testbench k v
testList testValues threadNums pNewLayers heights seeds =
    let actions=  [testAddAndRemove testValues t p h s | t <- threadNums
                                                       , p <- pNewLayers 
                                                       , h <- heights
                                                       , s  <- seeds ]
     in do forM_ actions id
           reportSuccess "Test add and remove."

getTestValues :: FilePath -> IO [Int]
getTestValues testFile =
    do rawValues <- readFile testFile
       return (map read $ lines rawValues)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a,b,c) = f a b c 
       
addTestbench :: Testbench k v
addTestbench testValues threadNums pNewLayers heights seeds =
    do let addTest splitValues threadNum  = 
             do forM_ [(pnl, h, s) | pnl <- pNewLayers, h <- heights, s <- seeds]
                      (uncurry3 $ testAdd splitValues testValues threadNum)
                reportSuccess $ "Test add with " ++ show threadNum ++ " threads."
       forM_ threadNums $ addTest True
       reportSuccess $ "Testing add done."
       addTest False (maximum threadNums)
       reportSuccess $ "Adding identical keys."

removeTestbench :: Testbench k v
removeTestbench testValues threadNums pNewLayers heights seeds =
    let removeTest splitValues threadNum =
          do forM_ [(pnl, h, s) | pnl <- pNewLayers, h <- heights, s <- seeds]
                   (uncurry3 $ testRemove splitValues testValues threadNum)
             reportSuccess $ "Test remove with " ++ show threadNum ++ " threads."
     in do forM_ threadNums (removeTest True)
           reportSuccess $ "Testing remove done."
           removeTest False (maximum threadNums)
           reportSuccess $ "Removing identical keys."

main :: IO ()
main =
    do let threadNums = [1..10]
           pNewLayers = [0.1, 0.25, 0.5, 0.75]
           heights    = [1..10]
           seeds      = [0..10]
       addTestbench testValues threadNums pNewLayers heights seeds
       removeTestbench testValues threadNums pNewLayers heights seeds
       testList testValues threadNums pNewLayers heights seeds
