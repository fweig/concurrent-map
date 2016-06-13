{- 
 - Simple utility to run benchmarks on either the skiplist
 - or binarytree.
 -}
module Main where 


import qualified BinaryTreeMap as BT
import qualified Skiplist as SL
import Fork

import Control.Concurrent
import Control.Exception.Base
import Control.Monad

import Data.Array.IO
import Data.IORef
import Data.List
import Data.Time

import System.CPUTime
import System.Environment
import System.Random
import System.IO.Unsafe


randSeeds = [ 476
            ,  24
            , 680
            , 125
            , 869
            , 77
            , 55
            , 556
            , 694
            , 149
            ]


-- Quick hack to ensure data read from a Map is actually used
sink :: IORef Bool
sink = unsafePerformIO $ newIORef False

-- Source: https://hackage.haskell.org/package/dsp-0.2.2/docs/src/Numeric-Statistics-Median.html#median
median :: (Ord a) => [a] -> a
median x =
   if odd n
     then sort x !! (n `div` 2)
     else (sort x !! (n `div` 2 - 1))
    where n = length x

-- Source: https://wiki.haskell.org/Random_shuffle
-- | Randomly shuffle a list
--   /O(N)/
shuffle :: [a] -> IO [a]
shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs =  newListArray (1,n) xs


time :: IO () -> IO NominalDiffTime
time io =
    do start <- getCurrentTime
       io
       end <- getCurrentTime
       return $ diffUTCTime end start

printResult :: Show a => a -> Double -> IO ()
printResult x t_ms = putStrLn (show x ++ " " ++ show t_ms)

parseArgs :: [String] -> (IO NominalDiffTime, String)
parseArgs ("skiplist":p:h:threadNum:instNum:scenario:name:_) = 
   (onSkiplist (read p) (read h) (read threadNum) (read instNum) scenario, name)
parseArgs ("binarytree":threadNum:instNum:scenario:name:_) = 
   (onBinaryTree (read threadNum) (read instNum) scenario, name)
parseArgs _ = error "Invalid arguments."

benchmark :: IO NominalDiffTime -> String -> IO ()
benchmark action name =
    do ts <- forM randSeeds $ \s -> do setStdGen (mkStdGen s)
                                       action 
       let m = median ts
       putStrLn (name ++ " " ++ show m)

actionOnSL :: SL.Skiplist Int () -> Int -> (Int, Int) -> Double -> Double -> IO ()
actionOnSL sl instNum keyRange lookupP addP =
    let actionOnSLacc rng x = let (r, rng') = random rng :: (Double, StdGen)
                                  (key, rng'') = randomR keyRange rng'
                                  detAction r key
                                     | (r <= lookupP) = void $ SL.contains sl key
                                     | (lookupP < r && r < lookupP + addP) = void $ SL.add sl key ()
                                     | (lookupP + addP <= r && r < 1) = void $ SL.remove sl key
                               in do detAction r key
                                     if x > 0
                                     then actionOnSLacc rng'' (x-1)
                                     else return ()
     in do rng <- newStdGen
           actionOnSLacc rng instNum

actionOnBT :: BT.BinaryTreeMap Int () -> Int -> (Int,Int) -> Double -> Double -> IO ()
actionOnBT bt instNum keyRange lookupP addP =
    let actionOnBTacc rng x = let (r, rng')    = random rng  :: (Double, StdGen)
                                  (key, rng'') = randomR keyRange rng' :: (Int, StdGen)
                                  detAction r key 
                                     | (r <= lookupP) = writeIORef sink =<< BT.contains bt key
                                     | (lookupP < r && r < lookupP + addP) = void $ BT.add bt key ()
                                     | (lookupP + addP <= r && r < 1) = void $ BT.remove bt key
                               in do detAction r key
                                     if x > 0
                                     then actionOnBTacc rng'' (x-1)
                                     else return ()
     in do rng <- newStdGen
           actionOnBTacc rng instNum

distributeOnThreads :: Int -> Int -> (Int -> IO ()) -> IO ()
distributeOnThreads threadNum instNum action =
    do let instPerThread = instNum `div` threadNum
       threaded (replicate threadNum $ action instPerThread)

threaded :: [IO ()] -> IO ()
threaded ios =
    do guards <- forM ios forkIOWait
       joinThreads guards

slices :: Int -> [a] -> [[a]]
slices 1 xs = xs:[]
slices pieces xs = let (p, xs') = splitAt (length xs `div` pieces) xs
                    in p : slices (pieces-1) xs'

onSkiplist :: Double -> Int -> Int -> Int -> String -> IO NominalDiffTime
onSkiplist pNewLayer height threadNum instNum scenario =
    do sl <- SL.new height pNewLayer
       let instPerThread = instNum `div` threadNum
           action = case scenario of
                      "add" -> (\x -> actionOnSL sl x (1,10^6) 0 1)
                      "mix1" -> (\x -> actionOnSL sl x (1,10^4) 0.1 0.6)
                      "mix2" -> (\x -> actionOnSL sl x (1,10^4) 0.4 0.6)
                      "mix3" -> (\x -> actionOnSL sl x (1,10^4) 0.0 0.5)
       when (scenario == "remove") $ forM_ [1..instNum] (\x -> SL.add sl x ())
       case scenario of
         "sorted-add" -> let actions = [forM_ [(t-1)*instPerThread+1..t*instPerThread] 
                                        $ \x -> SL.add sl x () 
                                           | t <- [1..threadNum]]
                          in time (threaded actions)
         "remove"     -> do toRemove <- shuffle [1..instNum]
                            let actions = [forM_ keys $ \x -> SL.remove sl x 
                                            | keys <- slices threadNum toRemove]
                            time (threaded actions)
         _            -> time (distributeOnThreads threadNum instNum action)



onBinaryTree :: Int -> Int -> String -> IO NominalDiffTime
onBinaryTree threadNum instNum scenario =
    do bt <- BT.new
       let instPerThread = instNum `div` threadNum
           action = case scenario of
                      "add" -> (\x -> actionOnBT bt x (1,10^6) 0 1)
                      "mix1" -> (\x -> actionOnBT bt x (1,10^4) 0.1 0.6)
                      "mix2" -> (\x -> actionOnBT bt x (1,10^4) 0.4 0.6)
                      "mix3" -> (\x -> actionOnBT bt x (1,10^4) 0.0 0.5)
       when (scenario == "remove") $ forM_ [1..instNum] (\x -> BT.add bt x ())
       case scenario of
         "sorted-add" -> let actions = [forM_ [(t-1)*instPerThread+1..t*instPerThread] 
                                        $ \x -> BT.add bt x () 
                                           | t <- [1..threadNum]]
                          in time (threaded actions)
         "remove"     -> do toRemove <- shuffle [1..instNum]
                            let actions = [forM_ keys $ \x -> BT.remove bt x 
                                            | keys <- slices threadNum toRemove]
                            time (do threaded actions
                                     x <- BT.eval bt
                                     return ())

         _            -> time (do distributeOnThreads threadNum instNum action
                                  x <- BT.eval bt
                                  return ())

main :: IO ()
main = do args <- getArgs 
          let (action, name) = parseArgs args
          benchmark action name
