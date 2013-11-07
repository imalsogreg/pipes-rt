module Main where

import Pipes
import Pipes.RealTime
import Control.Monad
import Data.Time
import Control.Concurrent
import System.IO

data TestTypeA = TestValueA Double String
               deriving (Show)

instance TMinus TestTypeA where
  tMinusSec (TestValueA t _) = t

data TestTypeB = TestValueB UTCTime String
               deriving (Show)

instance TimedEvent TestTypeB where
  timeOf (TestValueB t _) = t

-- Some test data that can associated relative timestamps
testDataA :: [TestTypeA]
testDataA = map (\t -> TestValueA t ("Data" ++ show t)) [0.5, 0.75..6]

-- Build some test data with associated absolute timestamps (using the t0 parameter,
-- so that the timestamps fall near the time that we run the example).
makeTestDataB :: UTCTime -> [TestTypeB]
makeTestDataB t0 = map (\(TestValueA t _) -> TestValueB (addUTCTime (doubleToDiff t) t0) "Payload") testDataA
  where doubleToDiff t =
          let tUTC =  UTCTime (ModifiedJulianDay 0) (picosecondsToDiffTime $ floor (t/1e-12))
              tZero = UTCTime (ModifiedJulianDay 0) (picosecondsToDiffTime 0)
          in diffUTCTime tUTC tZero


main :: IO ()
main = do

  hSetBuffering stdout NoBuffering

  putStrLn "\nGenerate some values at half-second intervals." >> drumRoll
  runEffect $ each [1..10 :: Int] >-> steadyCat 2 >-> printWithTime

  putStrLn "\nGenerate some values with poisson timing, 8 Hz" >> drumRoll
  runEffect $ each [1..20 :: Int] >-> poissonCat 8 >-> printWithTime
  threadDelay (truncate (1e6 :: Double))

  putStrLn "\nGenerate some values at their preferred times relative to now." >> drumRoll
  runEffect $ each testDataA >-> relativeTimeCat >-> printWithTime

  putStrLn "\nGenerate some values at their preferred absolute times." >> drumRoll
  do
    now <- getCurrentTime
    runEffect $ each (makeTestDataB now) >-> timeCat >-> printWithTime


printWithTime :: (Show a) => Consumer a IO r
printWithTime = forever $ do
  now <- lift getCurrentTime
  v <- await
  lift . putStrLn . unwords $ ["At time", show now, "got value", show v]

drumRoll :: IO ()
drumRoll = replicateM_ 3 (putStr "." >> threadDelay 500000) >> putStr "\n"


  
  