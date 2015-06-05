module Graphics.UI.Fui.Widget.Dynamic where

----------------------------------------------------------------------------

import Graphics.UI.Fui.Stream
import Graphics.UI.Fui.Sink
import Graphics.UI.Fui.GUI

----------------------------------------------------------------------------

import qualified Graphics.UI.Gtk as Gtk

----------------------------------------------------------------------------

dynamic :: Stream GUI -> GUI
dynamic srcGUI =
  MkGUI $
  do box <- Gtk.vBoxNew False 0
     wid <- pack Empty
     Gtk.containerAdd box wid
     flush srcGUI $ \(MkGUI gui) ->
       do Gtk.containerForeach box $ \wid ->
            do Gtk.containerRemove box wid
               --Gtk.widgetDestroy w
          lay <- gui
          wid <- pack lay
          Gtk.containerAdd box wid
          Gtk.widgetShowAll box
     return (Widget (Gtk.toWidget box))

----------------------------------------------------------------------------

