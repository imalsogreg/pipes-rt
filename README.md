pipes-rt
========

[![Build Status](https://travis-ci.org/imalsogreg/pipes-rt.svg?branch=master)](https://travis-ci.org/imalsogreg/pipes-rt.svg?branch=master)

A few pipes to yield values at a steady rate, as a poisson process, or according to the values themselves

For example:

```
λ: import Pipes
λ: import qualified Pipes.Prelude as PP
λ: import Pipes.RealTime
λ: import Data.Time
λ: import Data.Time.Clock 

λ: -- Pass values at 2 Hz
λ: runEffect $ each [1..4] >-> steadyCat 2 >-> PP.print
  [1/2 second pause...]
1 [1/2 second pause...]
2 [1/2 second pause...]
3 [1/2 second pause...]
4 [1/2 second pause...]

λ: -- Pass Values at 100 Hz, printing arrival times
λ: runEffect $ for (each [1..10] >-> steadyCat 100) (const $ lift (getCurrentTime >>= print))
2013-10-10 19:55:53.944484 UTC  
2013-10-10 19:55:53.954939 UTC      [          ]
2013-10-10 19:55:53.964623 UTC    [              ]
2013-10-10 19:55:53.975125 UTC  [   .. pauses ..    ]
2013-10-10 19:55:53.984759 UTC    [              ]
2013-10-10 19:55:53.994345 UTC      [          ]
2013-10-10 19:55:54.004886 UTC
2013-10-10 19:55:54.01449 UTC
2013-10-10 19:55:54.025124 UTC
2013-10-10 19:55:54.034661 UTC

λ: -- Pass values with Poisson timing, average 4 Hz, print data and arrival time
λ: runEffect $ for (each "Testing" >-> poissonCat 4) (\c -> lift (getCurrentTime >>= \t -> print (c,t)))
('T',2013-10-10 19:57:29.707621 UTC)
('e',2013-10-10 19:57:29.710815 UTC)
('s',2013-10-10 19:57:29.71766 UTC)
('t',2013-10-10 19:57:29.726371 UTC)
('i',2013-10-10 19:57:29.74401 UTC)
('n',2013-10-10 19:57:29.744338 UTC)
('g',2013-10-10 19:57:29.759882 UTC)

λ: -- Get timestamps from the data being piped
λ: import Data.Char
λ: let timeOfChar = (/ 10) . fromIntegral . (\c -> ord c - ord 'a')
λ: runEffect $ for (each "abcdwxyz" >-> relativeTimeCat timeOfChar) (\v -> lift (getCurrentTime >>= \t -> print (v,t)))
('a',2013-11-07 15:54:05.645025 UTC)   [ .. short pause .. ]
('b',2013-11-07 15:54:05.745847 UTC)   [ .. short pause .. ]
('c',2013-11-07 15:54:05.845771 UTC)   [ .. short pause .. ]
('d',2013-11-07 15:54:05.945533 UTC)   [ .. long  pause .. ]
('w',2013-11-07 15:54:07.847302 UTC)   [ .. short pause .. ]
('x',2013-11-07 15:54:07.946071 UTC)   [ .. short pause .. ]
('y',2013-11-07 15:54:08.045846 UTC)   [ .. short pause .. ]
('z',2013-11-07 15:54:08.145573 UTC)   [ .. short pause .. ]

λ: -- "Delay" the output by -2 seconds, which means
λ: -- skip ahead by 2 seconds
λ: let myPrint = (\v -> (getCurrentTime >>= \t -> print (v,t))) :: Char -> IO ()
λ: runEffect $ for (each "abcdwxyz" >-> relativeTimeCatDelayedBy timeOfChar (-0.2)) (lift . myPrint)
('c',2013-11-08 02:55:37.131626 UTC)
('d',2013-11-08 02:55:37.232347 UTC)   [ .. we discarded the early data .. ]
('w',2013-11-08 02:55:39.134008 UTC)   [ .. and jumped in immediately   .. ]  
('x',2013-11-08 02:55:39.232772 UTC)   [ .. at 'c'                      .. ]
('y',2013-11-08 02:55:39.332545 UTC)
('z',2013-11-08 02:55:39.432303 UTC)

```
