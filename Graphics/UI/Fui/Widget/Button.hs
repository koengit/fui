module Graphics.UI.Fui.Widget.Button where

----------------------------------------------------------------------------

import Graphics.UI.Fui.Stream
import Graphics.UI.Fui.Sink
import Graphics.UI.Fui.GUI

----------------------------------------------------------------------------

import qualified Graphics.UI.Gtk as Gtk

----------------------------------------------------------------------------

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

instance Sized Button where
  size (x,y) =
    MkProp (\(MkButton w _) ->
      do w `Gtk.onSizeRequest` return (Gtk.Requisition x y)
         return ()
    )

instance Active Button where
  activate = MkAttr
    (\(MkButton but chaText) snk ->
      do but `Gtk.onClicked` send snk ()
         return ()
    )
    (\(MkButton but chaText) src ->
      do flush src (\_ -> Gtk.buttonClicked but)
    )

instance Text Button where
  text = MkAttr
    (\(MkButton but chaText) snk ->
      do flush (source chaText) snk
    )
    (\(MkButton but chaText) src ->
      do flush src (Gtk.buttonSetLabel but)
         flush src (sink chaText)
    )

----------------------------------------------------------------------------

