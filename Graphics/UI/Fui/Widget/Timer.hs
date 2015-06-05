module Graphics.UI.Fui.Widget.Timer where

----------------------------------------------------------------------------

import Graphics.UI.Fui.Stream
import Graphics.UI.Fui.Sink
import Graphics.UI.Fui.GUI

----------------------------------------------------------------------------

import qualified Graphics.UI.Gtk as Gtk

----------------------------------------------------------------------------

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

