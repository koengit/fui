module OnceRef where

import Data.IORef

newtype OnceRef a =
  MkOnceRef (IORef (Maybe a))


