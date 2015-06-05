module Fui where

import qualified Graphics.UI.Gtk as Gtk
import Data.IORef

----------------------------------------------------------------------------
-- Sink

newtype Sink a
  = MkSink (a -> IO ())

sinkMap :: (b -> a) -> Sink a -> Sink b
sinkMap f (MkSink h) = MkSink (h . f)

send :: Sink a -> a -> IO ()
send (MkSink f) x = f x

mkSink :: (a -> IO ()) -> Sink a
mkSink f = MkSink f

hole :: Sink a
hole = MkSink (\_ -> return ())

(&) :: Sink a -> Sink a -> Sink a
MkSink f & MkSink g = MkSink (\x -> f x >> g x)

----------------------------------------------------------------------------
-- Event

newtype EventSource a
  = MkEventSource ((a -> IO ()) -> IO ())

instance Functor EventSource where
  f `fmap` MkEventSource src = MkEventSource (\k -> src (k . f))

triggers :: EventSource a -> (a -> IO ()) -> IO ()
MkEventSource trig `triggers` f = trig f

type Event a
  = (EventSource a, Sink a)

newOnceEvent :: IO (Event a)
newOnceEvent =
  do ref <- newIORef (Left (\_ -> return ()))

     let source f =
           do efa <- readIORef ref
              case efa of
                Left f' -> writeIORef ref (Left (\x -> f' x >> f x))
                Right x -> f x

         sink x =
           do efa <- readIORef ref
              case efa of
                Left f  -> do writeIORef ref (Right x)
                              f x
                Right _ -> do return ()

     return (MkEventSource source, MkSink sink)

----------------------------------------------------------------------------
-- Source

data Source a
  = Put a (Source a)
  | Choice [EventSource (Source a)]

instance Functor Source where
  f `fmap` Put x src   = Put (f x) (f `fmap` src)
  f `fmap` Choice evts = Choice (((f `fmap`) `fmap`) `map` evts)

put :: a -> Source a -> Source a
put x src = Put x src

wait :: EventSource a -> (a -> Source b) -> Source b
wait evt k = Choice [k `fmap` evt]

nil :: Source a
nil = Choice []

val :: a -> Source a
val x = put x nil

get :: Source a -> (a -> Source a -> Source b) -> Source b
get (Put x src)   f = f x src
get (Choice evts) f = Choice (((`get` f) `fmap`) `map` evts)

(><) :: Source a -> Source a -> Source a
Choice []           >< src2                = src2
src1                >< Choice []           = src1
Put x src1          >< src2                = Put x (src1 >< src2)
src1                >< Put x src2          = Put x (src1 >< src2)
src1@(Choice evts1) >< src2@(Choice evts2) =
  Choice ( ((>< src2) `fmap`) `map` evts1
        ++ ((>< src1) `fmap`) `map` evts2
         )

mapMaybeSource :: (a -> Maybe b) -> Source a -> Source b
mapMaybeSource f src =
  get src $ \x src' ->
    case f x of
      Just y  -> put y (mapMaybeSource f src)
      Nothing -> mapMaybeSource f src

flush :: Source a -> Sink a -> IO ()
flush (Put x src) snk =
  do send snk x
     flush src snk

flush (Choice []) snk =
  do return ()

flush (Choice [evt]) snk =
  do evt `triggers` \src -> flush src snk

flush (Choice evts) snk =
  do (evt',snk') <- newOnceEvent
     sequence_ [ evt `triggers` send snk' | evt <- evts ]
     evt' `triggers` \src -> flush src snk

----------------------------------------------------------------------------
-- Channel

newtype Stream a
  = MkStream (EventSource (a, Stream a))

newStream :: IO (Sink (a,Stream a), Stream a)
newStream =
  do (src,snk) <- newOnceEvent
     return (snk, MkStream src)

data Channel a
  = MkChannel (IORef (Sink (a,Stream a))) (Stream a)

newChannel :: IO (Channel a)
newChannel =
  do (snk,str) <- newStream
     ref       <- newIORef snk
     return (MkChannel ref str)

source :: Channel a -> Source a
source (MkChannel _ str) = src str
 where
  src (MkStream evt) = wait evt $ \(x,str) -> Put x (src str)

sink :: Channel a -> Sink a
sink (MkChannel ref _) =
  MkSink (\x -> do snk <- readIORef ref
                   (snk',str') <- newStream
                   writeIORef ref snk'
                   send snk (x,str'))

----------------------------------------------------------------------------
-- Layout

data Layout
  = Layout :<< Layout
  | Layout :^^ Layout
  | Widget Gtk.Widget
  | Empty

pack :: Layout -> IO Gtk.Widget
pack lay =
  case lay of
    Widget w -> return w
    _ :<< _  -> do box <- Gtk.hBoxNew False 3
                   sequence_ [ do w <- pack l
                                  Gtk.containerAdd box w
                             | l <- horiz lay 
                             ]
                   return (Gtk.toWidget box)
    _ :^^ _  -> do box <- Gtk.vBoxNew False 3
                   sequence_ [ do w <- pack l
                                  Gtk.containerAdd box w
                             | l <- verti lay 
                             ]
                   return (Gtk.toWidget box)
    Empty    -> do dummy <- Gtk.drawingAreaNew
                   dummy `Gtk.onSizeRequest` return (Gtk.Requisition 1 1)
                   return (Gtk.toWidget dummy)
 where
  horiz Empty       = []
  horiz (l1 :<< l2) = horiz l1 ++ horiz l2
  horiz w           = [w]

  verti Empty       = []
  verti (l1 :^^ l2) = verti l1 ++ verti l2
  verti w           = [w]

----------------------------------------------------------------------------
-- Attributes

data Attr w a
  = MkAttr (w -> Sink a -> IO ()) (w -> Source a -> IO ())

newtype Prop w
  = MkProp (w -> IO ())

{-
subAttr :: (w -> v) -> Attr v a -> Attr w a
f `subAttr` MkAttr fsnk fsrc = MkAttr (fsnk . f) (fsrc . f)

mapMaybeAttr :: (a -> Maybe b) -> (b -> a) -> Attr w a -> Attr w b
mapMaybeAttr f g (MkAttr fsnk fsrc) = MkAttr
  (\w snk -> fsnk w (mapSink g snk))
  (\w src -> fsrc w (mapMaybeSource f src))
-}

(+>) :: Attr w a -> Sink a -> Prop w
MkAttr fsnk _ +> snk = MkProp (\w -> fsnk w snk)

(<+) :: Attr w a -> Source a -> Prop w
MkAttr _ fsrc <+ src = MkProp (\w -> fsrc w src)

-- derived

(+>>) :: Attr w a -> Channel a -> Prop w
attr +>> ch = attr +> sink ch

(<<+) :: Attr w a -> Channel a -> Prop w
attr <<+ ch = attr <+ source ch

(<+=) :: Attr w a -> a -> Prop w
attr <+= x = attr <+ val x

-- specific attributes

class Active w where
  activate :: Attr w ()

class Text w where
  text :: Attr w String

class Container w where
  value :: Attr (w a) a

----------------------------------------------------------------------------
-- GUI

newtype GUI = MkGUI (IO Layout)

action :: IO GUI -> GUI
action ioGui = MkGUI (do MkGUI gui <- ioGui; gui)

empty :: GUI
empty = MkGUI (return Empty)

channel :: (Channel a -> GUI) -> GUI
channel f = action (do ch <- newChannel; return (f ch))

(.<<), (.^^) :: GUI -> GUI -> GUI
(.<<) = layoutCombo (:<<)
(.^^) = layoutCombo (:^^)

layoutCombo op (MkGUI gui1) (MkGUI gui2) =
  MkGUI $
  do ws1 <- gui1
     ws2 <- gui2
     return (ws1 `op` ws2)

run :: GUI -> IO ()
run (MkGUI gui) =
  do Gtk.initGUI
     gui
     Gtk.mainGUI

----------------------------------------------------------------------------
-- Window

data Window
  = MkWindow Gtk.Window (Channel String)

window :: [Prop Window] -> GUI -> GUI
window props (MkGUI gui) =
  MkGUI $
  do win <- Gtk.windowNew
     win `Gtk.onDestroy` Gtk.mainQuit
     chaTitle <- newChannel
     let w = MkWindow win chaTitle
     sequence_ [ prop w | MkProp prop <- props ]
     lay <- gui
     wid <- pack lay
     Gtk.containerAdd win wid
     Gtk.widgetShowAll win
     return Empty

instance Text Window where
  text = MkAttr
    (\(MkWindow win chaTitle) snk ->
      do flush (source chaTitle) snk
    )
    (\(MkWindow win chaTitle) src ->
      do flush src (mkSink (Gtk.windowSetTitle win))
         flush src (sink chaTitle)
    )

----------------------------------------------------------------------------
-- Label

data Label
  = MkLabel Gtk.Label (Channel String)

label :: [Prop Label] -> GUI
label props =
  MkGUI $
  do lab <- Gtk.labelNew Nothing
     chaText <- newChannel
     let w = MkLabel lab chaText
     sequence_ [ prop w | MkProp prop <- props ]
     return (Widget (Gtk.toWidget lab))

instance Text Label where
  text = MkAttr
    (\(MkLabel lab chaText) snk ->
      do flush (source chaText) snk
    )
    (\(MkLabel lab chaText) src ->
      do flush src (mkSink (Gtk.labelSetText lab))
         flush src (sink chaText)
    )

----------------------------------------------------------------------------
-- Timer

data Timer
  = MkTimer (Channel ())

timer :: Int -> [Prop Timer] -> GUI
timer t props =
  MkGUI $
  do chaAct <- newChannel
     Gtk.timeoutAdd (send (sink chaAct) () >> return True) t
     let w = MkTimer chaAct
     sequence_ [ prop w | MkProp prop <- props ]
     return Empty

instance Active Timer where
  activate = MkAttr
    (\(MkTimer chaAct) snk ->
      do flush (source chaAct) snk
    )
    (\(MkTimer chaAct) src ->
      do flush src (sink chaAct)
    )

----------------------------------------------------------------------------
-- Button

data Button
  = MkButton Gtk.Button (Channel String)

button :: [Prop Button] -> GUI
button props =
  MkGUI $
  do but <- Gtk.buttonNew
     chaText <- newChannel
     let w = MkButton but chaText
     sequence_ [ prop w | MkProp prop <- props ]
     return (Widget (Gtk.toWidget but))

instance Active Button where
  activate = MkAttr
    (\(MkButton but chaText) snk ->
      do but `Gtk.onClicked` send snk ()
         return ()
    )
    (\(MkButton but chaText) src ->
      do flush src (mkSink (\() -> Gtk.buttonClicked but))
    )

instance Text Button where
  text = MkAttr
    (\(MkButton but chaText) snk ->
      do flush (source chaText) snk
    )
    (\(MkButton but chaText) src ->
      do flush src (mkSink (Gtk.buttonSetLabel but))
         flush src (sink chaText)
    )

----------------------------------------------------------------------------
-- Entry

data Entry
  = MkEntry Gtk.Entry (Channel ()) (Channel String)

entry :: [Prop Entry] -> GUI
entry props =
  MkGUI $
  do ent <- Gtk.entryNew
     chaAct  <- newChannel
     chaText <- newChannel
     let w = MkEntry ent chaAct chaText
     sequence_ [ prop w | MkProp prop <- props ]
     return (Widget (Gtk.toWidget ent))

instance Active Entry where
  activate = MkAttr
    (\(MkEntry ent chaAct chaText) snk ->
      do ent `Gtk.onEntryActivate` send snk ()
         flush (source chaAct) snk
    )
    (\(MkEntry ent chaAct chaText) src ->
      do flush src (sink chaAct)
    )

instance Text Entry where
  text = MkAttr
    (\(MkEntry ent chaAct chaText) snk ->
      do ent `Gtk.onKeyRelease` \_ ->
           do s <- Gtk.entryGetText ent
              send snk s
              return False
         flush (source chaText) snk
    )
    (\(MkEntry ent chaAct chaText) src ->
      do flush src (mkSink (Gtk.entrySetText ent))
         flush src (sink chaText)
    )

----------------------------------------------------------------------------
-- TEntry

data TEntry a
  = MkTEntry Gtk.Entry (Channel ()) (Channel a) (a -> String) (String -> Maybe a)

tentry :: (Show a, Read a) => [Prop (TEntry a)] -> GUI
tentry props =
  MkGUI $
  do ent <- Gtk.entryNew
     chaAct <- newChannel
     chaVal <- newChannel
     let w = MkTEntry ent chaAct chaVal show readMaybe
     sequence_ [ prop w | MkProp prop <- props ]
     return (Widget (Gtk.toWidget ent))
 where
  readMaybe s = case reads s of
                  (x,""):_ -> Just x
                  _        -> Nothing

instance Active (TEntry a) where
  activate = MkAttr
    (\(MkTEntry ent chaAct chaVal show read) snk ->
      do ent `Gtk.onEntryActivate` send snk ()
         flush (source chaAct) snk
    )
    (\(MkTEntry ent chaAct chaVal show read) src ->
      do flush src (sink chaAct)
    )

instance Container TEntry where
  value = MkAttr
    (\(MkTEntry ent chaAct chaVal show read) snk ->
      do ent `Gtk.onKeyRelease` \_ ->
           do s <- Gtk.entryGetText ent
              case read s of
                Just x  -> send snk x
                Nothing -> return ()
              return False
         flush (source chaVal) snk
    )
    (\(MkTEntry ent chaAct chaVal show read) src ->
      do flush src (mkSink (Gtk.entrySetText ent . show))
         flush src (sink chaVal)
    )

----------------------------------------------------------------------------
-- Window

dynamic :: Source GUI -> GUI
dynamic srcGUI =
  MkGUI $
  do box <- Gtk.vBoxNew False 0
     flush (put empty srcGUI) $ mkSink $ \(MkGUI gui) ->
       do Gtk.containerForeach box $ \wid ->
            do Gtk.containerRemove box wid
               --Gtk.widgetDestroy w
          lay <- gui
          wid <- pack lay
          Gtk.containerAdd box wid
          Gtk.widgetShowAll box
     return (Widget (Gtk.toWidget box))

----------------------------------------------------------------------------

