module Graphics.UI.Fui.Widget.Label where

----------------------------------------------------------------------------

import Graphics.UI.Fui.Stream
import Graphics.UI.Fui.Sink
import Graphics.UI.Fui.GUI

----------------------------------------------------------------------------

import qualified Graphics.UI.Gtk as Gtk

----------------------------------------------------------------------------

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

instance Sized Label where
  size (x,y) =
    MkProp (\(MkLabel w _) ->
      do w `Gtk.onSizeRequest` return (Gtk.Requisition x y)
         return ()
    )

instance Text Label where
  text = MkAttr
    (\(MkLabel lab chaText) snk ->
      do flush (source chaText) snk
    )
    (\(MkLabel lab chaText) src ->
      do flush src (Gtk.labelSetText lab)
         flush src (sink chaText)
    )

----------------------------------------------------------------------------

