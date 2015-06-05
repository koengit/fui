module Graphics.UI.Fui.Time where

----------------------------------------------------------------------------

import Data.IORef
import System.CPUTime

----------------------------------------------------------------------------

data Time
  = MkTime (IORef (Either (IO ()) Integer))

newTime :: IO Time
newTime =
  do ref <- newIORef (Left (return ()))
     return (MkTime ref)

pin :: Time -> IO ()
pin (MkTime ref) =
  do eat <- readIORef ref
     case eat of
       Left action ->
         do t <- getCPUTime
            writeIORef ref (Right t)
            action

       Right _ -> -- already happened
         do return ()

at :: IO () -> Time -> IO ()
action `at` MkTime ref =
  do eat <- readIORef ref
     case eat of
       Left action' ->
         do writeIORef ref (Left (action' >> action))
       
       Right t -> -- already happened
         do action

inspect :: Time -> IO (Maybe Integer)
inspect (MkTime ref) =
  do eat <- readIORef ref
     return (case eat of
               Left _  -> Nothing
               Right t -> Just t)

----------------------------------------------------------------------------

