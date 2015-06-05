module Event
  ( Event    -- :: * -> *; Functor
  , never    -- :: Event a
  , (\/)     -- :: Event a -> Event a -> Event a
  , triggers -- :: Event a -> Sink a -> IO ()
  , newEvent -- :: IO (Event a, Sink a)
  )
 where

----------------------------------------------------------------------------

import Sink

----------------------------------------------------------------------------

import Data.IORef

----------------------------------------------------------------------------
-- event

-- events can happen at most once, and carry a value when they happen;
-- this value never changes once the event has happened

newtype Event a
  = MkEvent [Sink a -> IO ()]

instance Functor Event where
  f `fmap` MkEvent adds = MkEvent [ add . mapSink f | add <- adds ]

----------------------------------------------------------------------------

never :: Event a
never = MkEvent []

(\/) :: Event a -> Event a -> Event a
MkEvent adds1 \/ MkEvent adds2 = MkEvent (adds1 ++ adds2)

triggers :: Event a -> Sink a -> IO ()
MkEvent []    `triggers` snk = return ()
MkEvent [add] `triggers` snk = add snk
MkEvent adds  `triggers` snk =
  do (evt,trig) <- newEvent
     sequence_ [ add trig | add <- adds ]
     evt `triggers` snk

----------------------------------------------------------------------------

-- creating a new event, together with a sink that makes the event happen

newEvent :: IO (Event a, Sink a)
newEvent =
  do ref <- newIORef (Left hole)
     
     let evt = MkEvent [\snk ->
           do eta <- readIORef ref
              case eta of
                Left wait -> do writeIORef ref (Left (wait /\ snk))
                Right x   -> do send snk x
           ]
                 
         snk = sink (\x ->
           do eta <- readIORef ref
              case eta of
                Left wait -> do writeIORef ref (Right x)
                                send wait x
                Right _   -> do return () -- the event has already happened
           )
      
      in return (evt, snk)

----------------------------------------------------------------------------

