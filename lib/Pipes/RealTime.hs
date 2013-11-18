module Pipes.RealTime (

  -- *Pipes throttled by their own timestamps
  timeCat,
  timeCatDelayedBy,
  relativeTimeCat,
  relativeTimeCatDelayedBy,

  -- *Pipes throttled by you
  steadyCat,
  poissonCat,
  poissonCatConst,
  genPoissonCat,
  catAtTimes,
  catAtRelativeTimes,

  -- *Functions for interacting with playback
  relSeekAndWait,
  relSeekAndWaitV
  
  ) where

import Prelude hiding (dropWhile)
import Pipes
import Pipes.Prelude (chain, dropWhile)
import Control.Concurrent (threadDelay)
import Data.Time.Clock
import Data.Time.Calendar

import System.Random.MWC
import qualified System.Random.MWC.Distributions as MWCDists
import Control.Concurrent.STM

{-| Yield values some time after the effect is run,
    according to their relative timestamps.  Assumes that
    values arrive in ascending time order. Values with
    negative relative timestamps are discarded -}
relativeTimeCat :: (MonadIO m) => (a -> Double) -> Pipe a a m r
relativeTimeCat toRelTime = do
  t0 <- liftIO getCurrentTime
  dropWhile (( < 0 ) . toRelTime) >->
    chain (\v -> liftIO $ pauseUntil
                 (doubleToNomDiffTime (toRelTime v) `addUTCTime` t0))

{-| Yield values at their timestamps, but delay
    by some time (given in seconds).  Passing
    a negative delay advances the generator,
    discarding events happening before the effect -}
relativeTimeCatDelayedBy :: (MonadIO m) => (a -> Double) -> Double
                            -> Pipe a a m r
relativeTimeCatDelayedBy toTime delay = relativeTimeCat toTime'
     where toTime' = ((+ delay) . toTime)

{-| Yield values at the absolute times given by their timestamps.
    Assumes that they arrive in ascending time order. Values with timestamps
    earlier than the starting time of the effect are discarded -}
timeCat :: (MonadIO m) => (a -> UTCTime) -> Pipe a a m r
timeCat toTime = do
  t0 <- liftIO getCurrentTime
  dropWhile (( < t0 ) . toTime) >->
    chain (liftIO . pauseUntil . toTime)

{-| Yield values at their absolute timesteps, but delay
    or advance their production by some time (given in
    seconds).  Values with timestamps less than zero
    after adjustment are discarded -}
timeCatDelayedBy :: (MonadIO m) => (a -> UTCTime) -> Double -> Pipe a a m r
timeCatDelayedBy toTime delay = do
  timeCat $ toTime'
  where toTime' = (doubleToNomDiffTime delay `addUTCTime`) . toTime
  
{-| Yield values at steady rate (Hz) -}
steadyCat :: (MonadIO m) => Double -> Pipe a a m r
steadyCat rate = do
  t0 <- liftIO getCurrentTime
  loop t0
  where
    dtUTC = doubleToNomDiffTime (1/rate)
    loop t =
      let t' = dtUTC `addUTCTime` t in do
        liftIO $ pauseUntil t'
        v <- await
        yield v
        loop t'

{-| Constant-rate Poisson process yielding values, randomized by IO -}
poissonCat :: (MonadIO m) => Double -> Pipe a a m r
poissonCat rate = liftIO createSystemRandom >>= \gen ->
  genPoissonCat gen rate

{-| Constant-rate Poisson process with a fixed seed -
    the same random every time -}
poissonCatConst :: (MonadIO m) => Double -> Pipe a a m r
poissonCatConst rate = liftIO create >>=  \gen ->
  genPoissonCat gen rate

{-| Constant-rate Poisson process yielding values, seeded by you -}
genPoissonCat :: (MonadIO m) => GenIO -> Double -> Pipe a a m r
genPoissonCat gen rate = do
  t0 <- liftIO getCurrentTime
  loop t0
  where
    loop t = do
      v <- await
      dt <- liftIO $ MWCDists.exponential rate gen
      let t' = addUTCTime (doubleToNomDiffTime dt) t
      liftIO $ pauseUntil t'
      yield v
      loop t'

{-|Yield values at a set of absolute times.
   Yield remaining values immediately if the
   time list becomes empty -}
catAtTimes :: (MonadIO m) => [UTCTime] -> Pipe a a m r
catAtTimes []     = cat
catAtTimes (t:ts) = do
  liftIO $ pauseUntil t
  v <- await
  yield v
  catAtTimes ts

{-|Yield values at a set of times relative to the first received value.
   Yield remaining values immediately if the time list becomes empty -}
catAtRelativeTimes :: (MonadIO m) => [Double] -> Pipe a a m r
catAtRelativeTimes []       = cat
catAtRelativeTimes ts@(_:_) = liftIO absTimes >>= catAtTimes 
  where absTimes = 
          getCurrentTime >>= \t0 ->
          return $ map (\d -> doubleToNomDiffTime d `addUTCTime` t0) ts

{-|Drop values until the target relative timestamp is reached, then
   wait until another thread writes () to the TMVar before proceeding
   to yield values according to their timestamps -}
relSeekAndWait :: (MonadIO m) =>
                  TMVar () ->
                  Double ->
                  (a -> Double) ->
                  Pipe a a m r
relSeekAndWait signal time toTime = do
  dropWhile ( (< time) . toTime)
  () <- liftIO . atomically . readTMVar $ signal
  relativeTimeCat toTime

relSeekAndWaitV :: (MonadIO m) =>
                   TMVar () ->
                   Double ->
                   (a -> Double) ->
                   Pipe a a m r
relSeekAndWaitV signal time toTime = do
  dropWhile ( (< time) . toTime)
  liftIO $ putStrLn "Seek done. Waiting for signal."
  () <- liftIO . atomically . readTMVar $ signal
  relativeTimeCat toTime

pauseUntil :: UTCTime -> IO ()
pauseUntil t = do
  now <- getCurrentTime
  case compare now t of
    LT -> threadDelay (truncate (diffUTCTime t now * 1000000))
    _  -> return ()

doubleToNomDiffTime :: Double -> NominalDiffTime
doubleToNomDiffTime x =
  let d0 = ModifiedJulianDay 0
      t0 = UTCTime d0 (picosecondsToDiffTime 0)
      t1 = UTCTime d0 (picosecondsToDiffTime $ floor (x/1e-12))
  in  diffUTCTime t1 t0
