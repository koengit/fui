module Graphics.UI.Fui.GUI where

----------------------------------------------------------------------------

import Graphics.UI.Fui.Stream
import Graphics.UI.Fui.Sink

----------------------------------------------------------------------------

import qualified Graphics.UI.Gtk as Gtk

----------------------------------------------------------------------------

infix  3 +>, <+

----------------------------------------------------------------------------
-- layout

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
-- attr

data Attr w a
  = MkAttr (w -> Sink a -> IO ()) (w -> Stream a -> IO ())

newtype Prop w
  = MkProp (w -> IO ())

(+>) :: Attr w a -> Sink a -> Prop w
MkAttr fsnk _ +> snk = MkProp (\w -> fsnk w snk)

(<+) :: Attr w a -> Stream a -> Prop w
MkAttr _ fsrc <+ src = MkProp (\w -> fsrc w src)

{-
-- derived

(+>>) :: Attr w a -> Channel a -> Prop w
attr +>> ch = attr +> sink ch

(<<+) :: Attr w a -> Channel a -> Prop w
attr <<+ ch = attr <+ source ch

(<+=) :: Attr w a -> a -> Prop w
attr <+= x = attr <+ val x
-}

-- specific attributes

class Sized w where
  size :: (Int,Int) -> Prop w

class Active w where
  activate :: Attr w ()

class Text w where
  text :: Attr w String

class Container w where
  value :: Attr (w a) a

----------------------------------------------------------------------------
-- channel

data Channel a = MkChannel{ source :: Stream a, sink :: Sink a }

newChannel :: IO (Channel a)
newChannel =
  do (src,snk) <- newStream
     return (MkChannel src snk)

channel :: (Channel a -> GUI) -> GUI
channel f = action (do ch <- newChannel; return (f ch))

----------------------------------------------------------------------------
-- GUI

newtype GUI = MkGUI (IO Layout)

action :: IO GUI -> GUI
action ioGui = MkGUI (do MkGUI gui <- ioGui; gui)

empty :: GUI
empty = MkGUI (return Empty)

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


