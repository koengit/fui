module Event1
  ( Event1   -- :: * -> *; Functor
  , never    -- :: Event1 a
  , triggers -- :: Event1 a -> Sink a -> IO ()
  , newEvent -- :: IO (Event1 a, Sink a)
  )
 where

----------------------------------------------------------------------------

import Sink

----------------------------------------------------------------------------

import Data.IORef
import System.IO.Unsafe( unsafePerformIO )

----------------------------------------------------------------------------
-- event

data Event1 a
  = Never
  | Happened a
  | Wait (IORef (Waiting a))

data Waiting a
  = Waiting (Sink a)
  | Over a

instance Functor Event1 where
  fmap f Never          = Never
  fmap f (Happened x)   = Happened (f x)
  fmap f evt@(Wait ref) = unsafePerformIO $
    do w <- readIORef ref
       case w of
         Waiting snk -> do (evt',snk') <- newEvent
                           evt `triggers` sink (\x -> send snk' (f x))
                           return evt'
         Over x      -> do return (Happened (f x))

----------------------------------------------------------------------------

never :: Event1 a
never = Never

triggers :: Event1 a -> Sink a -> IO ()
Never      `triggers` snk = return ()
Happened x `triggers` snk = send snk x
Wait ref   `triggers` snk =
  do w <- readIORef ref
     case w of
       Waiting snk' -> do writeIORef ref (Waiting (snk' /\ snk))
       Over x       -> do send snk x

----------------------------------------------------------------------------

-- creating a new event, together with a sink that makes the event happen

newEvent :: IO (Event1 a, Sink a)
newEvent =
  do ref <- newIORef (Waiting hole)
     
     let snk = sink (\x ->
           do w <- readIORef ref
              case w of
                Waiting snk' -> do writeIORef ref (Over x)
                                   send snk' x
                _            -> do return () -- the event has already happened
           )
      
      in return (Wait ref, snk)

----------------------------------------------------------------------------

