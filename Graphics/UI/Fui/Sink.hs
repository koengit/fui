module Graphics.UI.Fui.Sink
  ( Sink    -- :: * -> *
  , mapSink -- :: (b -> a) -> Sink a -> Sink b
  , hole    -- :: Sink a
  , send    -- :: Sink a -> a -> IO ()
  , (/\)    -- :: Sink a -> Sink a -> Sink a
  , (>+), (>!+), stdio, stdout
  )
 where

infixr 4 >+, >!+

----------------------------------------------------------------------------
-- sink

-- sinks are actions that can receive values

type Sink a
  = a -> IO ()

----------------------------------------------------------------------------

mapSink, (>+) :: (b -> a) -> Sink a -> Sink b
mapSink f k = k . f
(>+)        = mapSink

(>!+) :: (b -> IO a) -> Sink a -> Sink b
f >!+ snk = \x -> do y <- f x; snk y

hole :: Sink a
hole = \_ -> return ()

send :: Sink a -> a -> IO ()
send k = k

(/\) :: Sink a -> Sink a -> Sink a
k1 /\ k2 = \x -> k1 x >> k2 x

stdio :: Sink (IO ())
stdio = id

stdout :: Show a => Sink a
stdout = print

----------------------------------------------------------------------------

