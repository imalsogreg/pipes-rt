pipes-rt
========

A few pipes to yield values in timed patterns, and typeclasses to give values timestamps.

For example:

```
λ: import Pipes
λ: import qualified Pipes.Prelude as PP
λ: import Pipes.RealTime
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
λ: instance TMinus Char where tMinusSec = (/ 10) . fromIntegral . (\c -> ord c - ord 'a')
λ: runEffect $ for (each "abcdwxyz" >-> relativeTimeCat ) (\v -> lift (getCurrentTime >>= \t -> print (v,t)))
('a',2013-10-10 21:53:38.555041 UTC)
('b',2013-10-10 21:53:38.65557 UTC)
('c',2013-10-10 21:53:38.756121 UTC)
('d',2013-10-10 21:53:38.855951 UTC)
('w',2013-10-10 21:53:40.756643 UTC)
('x',2013-10-10 21:53:40.855328 UTC)
('y',2013-10-10 21:53:40.955936 UTC)
('z',2013-10-10 21:53:41.055523 UTC)


```
