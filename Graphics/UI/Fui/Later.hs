module Graphics.UI.Fui.Later where

----------------------------------------------------------------------------

import Data.IORef
import System.IO.Unsafe( unsafeInterleaveIO ) -- oh, well

----------------------------------------------------------------------------
{-

The function "later" is a variant of the function fixIO. It provides access
to a value before the value is actually defined. When we create the value,
we also create a function that needs to be used to define the value:

  later :: IO (a, a -> IO ())

If the value is evaluated before it is defined, then the program loops
(much like in other programs that are too strict).

Typical use:

  do (x,def_x) <- later

     .. do something with x (without evaluating it) ..

     def_x 42

     .. now we can evaluate x, which has the value 42 ..

Example 1 (OK use):

  do (xs,def_xs) <- later
     def_xs ('a' : xs)
     print (take 50 xs)

Example 2 (OK use):

  do (xs,def_xs) <- later
     def_xs ('a' : xs)
     def_xs "cow" -- this is ignored, only the first definition counts
     print (take 50 xs)

Example 3 (bad use (s is never defined), loops):

  do (s,def_s) <- later
     print (s :: String)

Example 4 (bad use (too strict in x), loops):

  do (x,def_x) <- later
     print x
     def_x 42

Example 5 (bad use (too strict in x), loops):

  do (x,def_x) <- later
     if x then def_x False
          else def_x True
     print (x :: Bool)

-}
----------------------------------------------------------------------------
-- later

later :: IO (a, a -> IO ())
later =
  do ref <- newIORef Nothing
     
     let get =
           do mx <- readIORef ref
              case mx of
                Just x  -> return x
                Nothing -> loop
     
         def x =
           do mx <- readIORef ref
              case mx of
                Just _  -> return ()
                Nothing -> writeIORef ref (Just x)
     
     x <- unsafeInterleaveIO get
     return (x, def)
 where
  -- this should really loop, but that is so hard to debug!
  loop = error "sooner rather than later"

----------------------------------------------------------------------------

