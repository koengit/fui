module Graphics.UI.Fui.Widget.Window where

----------------------------------------------------------------------------

import Graphics.UI.Fui.Stream
import Graphics.UI.Fui.Sink
import Graphics.UI.Fui.GUI

----------------------------------------------------------------------------

import qualified Graphics.UI.Gtk as Gtk

----------------------------------------------------------------------------

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

instance Sized Window where
  size (x,y) =
    MkProp (\(MkWindow w _) ->
      do w `Gtk.onSizeRequest` return (Gtk.Requisition x y)
         return ()
    )

instance Text Window where
  text = MkAttr
    (\(MkWindow win chaTitle) snk ->
      do flush (source chaTitle) snk
    )
    (\(MkWindow win chaTitle) src ->
      do flush src (Gtk.windowSetTitle win)
         flush src (sink chaTitle)
    )

----------------------------------------------------------------------------

