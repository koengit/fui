module Graphics.UI.Fui.Stream
  ( Stream    -- :: * -> *; Functor
  , nil       -- :: Stream a
  , put       -- :: a -> Stream a -> Stream a
  , val       -- :: a -> Stream a
  , get       -- :: Stream a -> (a -> Stream a -> Stream b) -> Stream b
  , (><)      -- :: Stream a -> Stream a -> Stream a
  , wait      -- :: Event a -> (a -> Stream b) -> Stream b
  , flush     -- :: Stream a -> Sink a -> IO ()
  , newStream -- :: IO (Stream a, Sink a)
  
  , sync', when', sync, when, fold, (+<)
  )
 where

----------------------------------------------------------------------------

import Graphics.UI.Fui.Sink
import Graphics.UI.Fui.Event
import Control.Exception

----------------------------------------------------------------------------

import Data.IORef

----------------------------------------------------------------------------

infixl 4 +<

----------------------------------------------------------------------------
-- stream

-- a stream is a possibly infinite stream of values, that can wait
-- for events every now and then

data Stream a
  = a :> Stream a
  | Wait (Event (Stream a))

instance Functor Stream where
  fmap f (x :> str) = f x :> fmap f str
  fmap f (Wait evt) = Wait (fmap f `fmap` evt)

(+<) :: Functor f => (a -> b) -> f a -> f b
f +< m = fmap f m

----------------------------------------------------------------------------

put :: a -> Stream a -> Stream a
put x str = x :> str

nil :: Stream a
nil = Wait never

val :: a -> Stream a
val x = put x nil

wait :: Event a -> (a -> Stream b) -> Stream b
wait evt f = Wait (f `fmap` evt)

get :: Stream a -> (a -> Stream a -> Stream b) -> Stream b
get (x :> str) f = f x str
get (Wait evt) f = wait evt (\str' -> get str' f)

(><) :: Stream a -> Stream a -> Stream a
(x :> str1)      >< str2             = x :> (str1 >< str2)
str1             >< (y :> str2)      = y :> (str1 >< str2)
str1@(Wait evt1) >< str2@(Wait evt2) =
  Wait (fmap (>< str2) evt1 \/ fmap (str1 ><) evt2)

sync' :: a -> Stream a -> Stream b -> Stream (a,b)
sync' a as bs = find a (fmap Left as >< fmap Right bs)
 where
  find a s = get s $ \eab s' -> case eab of
                                  Left a' -> find a' s'
                                  Right b -> put (a,b) (find a s')

when' :: a -> Stream a -> Stream b -> Stream a
when' a as bs = fmap fst (sync' a as bs)

sync :: Stream a -> Stream b -> Stream (a,b)
sync = sync' (error "sync")

when :: Stream a -> Stream b -> Stream a
when = when' (error "when")

fold :: (a -> b -> b) -> b -> Stream a -> Stream b
fold op x as = put x $ get as $ \a as' -> fold op (a `op` x) as' 

snapshot :: Stream a -> Stream (Stream a)
snapshot s = get s $ \x s' -> put s (snapshot s')

join :: Stream (Stream a) -> Stream a
join ssa = get ssa $ \sa ssa' -> sa >< join ssa'

----------------------------------------------------------------------------
-- stream --> sink

-- sending all elements in a stream to a sink (as soon as they are available)

flush :: Stream a -> Sink a -> IO ()
flush (x :> str) snk = do send snk x; flush str snk
flush (Wait evt) snk = do evt `triggers` \str -> flush str snk

----------------------------------------------------------------------------
-- sink --> stream

-- creating a stream, together with a sink that makes elements appear
-- in the stream

newStream :: IO (Stream a, Sink a)
newStream =
  do (evt,arr) <- newEvent
     ref <- newIORef arr
     let snk x =
           do arr <- readIORef ref
              (evt',arr') <- newEvent
              writeIORef ref arr'
              send arr (x :> Wait evt')
      in return (Wait evt,snk)

----------------------------------------------------------------------------

