{-# LANGUAGE GADTs #-}
module Main where

import qualified Graphics.UI.Gtk as Gtk
import Data.IORef
import System.IO.Unsafe

-- Event

newtype Event a
  = MkEvent (IORef (Either (a -> IO ()) a))

newEvent :: IO (Event a)
newEvent =
  do ref <- newIORef (Left (\_ -> return ()))
     return (MkEvent ref)

occurs :: Event a -> a -> IO ()
occurs (MkEvent ref) x =
  do efa <- readIORef ref
     case efa of
       Left f  -> do writeIORef ref (Right x)
                     f x
       Right _ -> do return ()

trigger :: Event a -> (a -> IO ()) -> IO ()
trigger (MkEvent ref) f =
  do efa <- readIORef ref
     case efa of
       Left f' -> writeIORef ref (Left (\x -> f' x >> f x))
       Right x -> f x

{-
buttonEvent :: Button -> Event ()
buttonEvent but =
  do evt <- newEvent
     but `onClicked` occurs evt ()
     return evt
-}

--

newtype Events a
  = Occurs (Event (a,Events a))

newEvents :: IO (Events a)
newEvents = Occurs `fmap` newEvent

data Channel a
  = MkChannel (IORef (Events a)) (Events a)

newChannel :: IO (Channel a)
newChannel =
  do evts <- newEvents
     ref  <- newIORef evts
     return (MkChannel ref evts)

inPort :: Channel a -> (a -> IO ())
inPort (MkChannel ref _) = \x ->
  do Occurs evt <- readIORef ref
     evts <- newEvents
     writeIORef ref evts
     occurs evt (x,evts)

outPort :: Channel a -> Events a
outPort (MkChannel _ evts) = evts

--     
    
data Stream a where
  Put  :: a -> Stream a -> Stream a
  Wait :: Event b -> (b -> Stream a) -> Stream a
  Nil  :: Stream a

instance Functor Stream where
  fmap f (Put x str)  = Put (f x) (fmap f str)
  fmap f (Wait evt k) = Wait evt (fmap f . k)
  fmap f Nil          = Nil

put  = Put
wait = Wait
nil  = Nil

val :: a -> Stream a
val x = put x nil

get :: Stream b -> (b -> Stream b -> Stream a) -> Stream a
get (Put x str)  f = f x str
get (Wait evt k) f = Wait evt (\x -> get (k x) f)
get Nil          f = Nil

stream :: Channel a -> Stream a
stream ch = str (outPort ch)
 where
  str (Occurs evt) = wait evt (\(x,evts) -> put x (str evts))

process :: Stream a -> (a -> IO ()) -> IO ()
process (Put x str)  f = f x >> process str f
process (Wait evt k) f = trigger evt (\x -> process (k x) f)
process Nil          f = return ()

-- widgets

type GUI = IO [Gtk.Widget]

channel :: (Channel a -> GUI) -> GUI
channel f =
  do ch <- newChannel
     f ch

window :: Stream String -> GUI -> GUI
window strTitle gui =
  do win <- Gtk.windowNew
     win `Gtk.onDestroy` Gtk.mainQuit
     process strTitle $ \s ->
       Gtk.windowSetTitle win s
     ws <- gui
     lay <- Gtk.vBoxNew False 5
     sequence_ [ Gtk.containerAdd lay w | w <- ws ]
     Gtk.containerAdd win lay
     Gtk.widgetShowAll win
     return []

button :: Stream String -> Channel () -> GUI
button strLabel chClick =
  do but <- Gtk.buttonNew
     but `Gtk.onClicked` inPort chClick ()
     process strLabel $ \s ->
       Gtk.buttonSetLabel but s
     return [Gtk.toWidget but]

dynamic :: Stream GUI -> GUI
dynamic guis =
  do lay <- Gtk.vBoxNew False 0
     process guis $ \gui ->
       do Gtk.containerForeach lay $ \w ->
            do Gtk.containerRemove lay w
               Gtk.widgetDestroy w
          ws <- gui
          sequence_ [ Gtk.containerAdd lay w | w <- ws ]
          Gtk.widgetShowAll lay
     return [Gtk.toWidget lay]

(<<) :: GUI -> GUI -> GUI
gui1 << gui2 =
  do ws1 <- gui1
     ws2 <- gui2
     return (ws1 ++ ws2)

run :: GUI -> IO ()
run gui =
  do Gtk.initGUI
     gui
     Gtk.mainGUI

-- example

main :: IO ()
main = run example

example :: GUI
example =
  channel $ \clicks ->
    let number :: Stream Int
        number = count 1 (stream clicks)
        
        label :: Stream String
        label = fmap show number
        
        buttons :: Stream GUI
        buttons = fmap (\k -> foldr1 (<<) [ button label clicks | i <- [1..k] ]) number
     in
      window (val "Click me")
      ( button label clicks
      )
     <<
      window (fmap (++ " buttons!") label)
      ( button (fmap (`replicate` '*') number) clicks
     << dynamic buttons
      )

count :: Int -> Stream () -> Stream Int
count n str = put n (get str (\_ str' -> count (n+1) str'))

