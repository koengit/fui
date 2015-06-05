module Main where

import Graphics.UI.Gtk

main :: IO ()
main =
  do initGUI
     win <- windowNew
     
     -- canvas
     can <- drawingAreaNew
     can `onSizeRequest` return (Requisition 400 400)
     can `onExpose` updateCanvas can ent
     
     -- button
     but <- buttonNew
     set but [ buttonLabel := "Quit" ]
     onClicked but mainQuit
     
     -- entry
     ent <- entryNew
     
     -- layout
     lay <- vBoxNew False 5
     containerAdd lay can
     containerAdd lay ent
     containerAdd lay but
     
     set win [ containerBorderWidth := 10
             , containerChild := lay
             ]
     onDestroy win mainQuit
     widgetShowAll win
     mainGUI

updateCanvas :: DrawingArea -> Entry -> Event -> IO Bool
updateCanvas can ent _evt =
  do win     <- widgetGetDrawWindow can
     (wd,ht) <- widgetGetSize can
     gc      <- gcNew win
     gcSetValues gc $ newGCValues {
       foreground = Color 65535 0 0,
       capStyle = CapRound,
       lineWidth  = 20,
       joinStyle = JoinRound
     }
     drawLines win gc [(30,30),(wd-30,ht-30),(wd-30,30),(30,ht-30)]
     return True

