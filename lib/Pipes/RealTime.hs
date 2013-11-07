module Pipes.RealTime (

  -- *Pipes throttled by their own timestamps
  timeCat,
  relativeTimeCat,
  dropExpired,
  dropRelativeExpired,

  -- *Pipes throttled by you
  steadyCat,
  poissonCat,
  poissonCatConst,
  genPoissonCat,
  catAtTimes,
  catAtRelativeTimes,

  -- *Discard leftover result
  dropResult
  
  ) where

import Control.Monad
import Pipes
import Control.Concurrent (threadDelay)
import Data.Time.Clock
import Data.Time.Calendar

import System.Random.MWC
import qualified System.Random.MWC.Distributions as MWCDists

{-| Yield values some time after the effect is run,
    according to their relative timestamps.  Assumes that
    values arrive in ascending time order -}
relativeTimeCat :: (a -> Double) -> Pipe a a IO r
relativeTimeCat toTime = do
  t0 <- lift getCurrentTime
  forever $ do
    v <- await
    lift $ pauseUntil (doubleToNomDiffTime (toTime v) `addUTCTime` t0)
    yield v

{-| Yield values at the absolute times given by their timestamps.
    Assumes that they arrive in ascending time order. Values with timestamps
    earlier than the starting time of the effect are yielded immediately -}
timeCat :: (a -> UTCTime) -> Pipe a a IO r
timeCat toRelTime = forever $ do
    v <- await
    lift $ pauseUntil (toRelTime v)
    yield v

{-| Discard events whose timestamps occur before the effect started running,
    instead of yielding them -}
dropExpired :: (a -> UTCTime) -> Pipe a a IO ()
dropExpired toTime = do
  v <- await
  now <- lift getCurrentTime
  case compare now (toTime v) of
    GT -> dropExpired toTime
    _  -> return ()

{-| Discard events whose relative timestamps are less than 0 -}
dropRelativeExpired :: (Monad m) => (a -> Double) -> Pipe a a m ()
dropRelativeExpired toRelTime = do
  v <- await
  when (toRelTime v < 0) (dropRelativeExpired toRelTime)
  
{-| Yield values at steady rate (Hz) -}
steadyCat :: Double -> Pipe a a IO r
steadyCat rate = do
  t0 <- lift getCurrentTime
  aux t0
  where
    dtUTC = doubleToNomDiffTime (1/rate)
    aux t =
      let t' = dtUTC `addUTCTime` t in do
        lift $ pauseUntil t'
        v <- await
        yield v
        aux t'

{-| Constant-rate Poisson process yielding values, randomized by IO -}
poissonCat :: Double -> Pipe a a IO r
poissonCat rate = lift createSystemRandom >>= \gen -> genPoissonCat gen rate

{-| Constant-rate Poisson process with a fixed seed - the same random every time -}
poissonCatConst :: Double -> Pipe a a IO r
poissonCatConst rate = lift create >>=  \gen -> genPoissonCat gen rate

{-| Constant-rate Poisson process yielding values, seeded by you -}
genPoissonCat :: GenIO -> Double -> Pipe a a IO r
genPoissonCat gen rate = do
  t0 <- lift getCurrentTime
  loop t0
  where
    loop t = do
      v <- await
      dt <- lift $ MWCDists.exponential rate gen
      let t' = addUTCTime (doubleToNomDiffTime dt) t
      lift $ pauseUntil t'
      yield v
      loop t'

{-|Yield values at a set of absolute times.
   Yield remaining values immediately if the
   time list becomes empty -}
catAtTimes :: [UTCTime] -> Pipe a a IO r
catAtTimes []     = cat
catAtTimes (t:ts) = do
  lift $ pauseUntil t
  v <- await
  yield v
  catAtTimes ts

{-|Yield values at a set of times relative to the first received value.
   Yield remaining values immediately if the time list becomes empty -}
catAtRelativeTimes :: [Double] -> Pipe a a IO r
catAtRelativeTimes []       = cat
catAtRelativeTimes ts@(_:_) = lift absTimes >>= catAtTimes 
  where absTimes = 
          getCurrentTime >>= \t0 ->
          return $ map (\d -> doubleToNomDiffTime d `addUTCTime` t0) ts


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

dropResult :: (Monad m) => Proxy a' a b' b m r -> Proxy a' a b' b m ()
dropResult p = p >>= \_ -> return ()