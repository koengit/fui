module Graphics.UI.Fui.Event
  ( Event    -- :: * -> *; Functor
  , never    -- :: Event a
  , (\/)     -- :: Event a -> Event a -> Event a
  , triggers -- :: Event a -> Sink a -> IO ()
  , newEvent -- :: IO (Event a, Sink a)
  )
 where

----------------------------------------------------------------------------

import Graphics.UI.Fui.Sink
import Graphics.UI.Fui.Later
import Graphics.UI.Fui.Time

----------------------------------------------------------------------------

data Event a
  = Later Time a
  | Event a :\/ Event a
  | Never

instance Functor Event where
  f `fmap` Never       = Never
  f `fmap` Later t x   = Later t (f x)
  f `fmap` (e1 :\/ e2) = (f `fmap` e1) :\/ (f `fmap` e2)

newEvent :: IO (Event a, Sink a)
newEvent =
  do t       <- newTime
     (x,def) <- later
     return (Later t x, \x -> do def x; pin t)

occurred :: Event a -> IO (Maybe (Integer,a))
occurred Never       = return Nothing
occurred (Later t x) = fmap (\n -> (n,x)) `fmap` inspect t
occurred (e1 :\/ e2) =
  do mtx1 <- occurred e1
     mtx2 <- occurred e2
     case (mtx1, mtx2) of
       (Just (t1,x1), Just (t2,x2))
         | t1 <= t2                 -> return mtx1
         | otherwise                -> return mtx2
       (Just (t1,x2), _)            -> return mtx1
       (_           , Just (t2,x2)) -> return mtx2
       (_           , _)            -> return Nothing

triggers :: Event a -> (a -> IO ()) -> IO ()
Never           `triggers` _ = return ()
Later t x       `triggers` k = k x `at` t
evt@(e1 :\/ e2) `triggers` k =
  do mtx <- occurred evt
     case mtx of
       Just (_,x) -> k x
       Nothing    -> do (evt',arr') <- newEvent
                        evt' `triggers` k
                        e1   `triggers` arr'
                        e2   `triggers` arr'

never :: Event a
never = Never

(\/) :: Event a -> Event a -> Event a
Never \/ evt   = evt
evt   \/ Never = evt
evt1  \/ evt2  = evt1 :\/ evt2

----------------------------------------------------------------------------

