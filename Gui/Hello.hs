module Main where

import Graphics.UI.Gtk

------------------------------------------------------------------------

main :: IO ()
main =
  do initGUI
     
     -- create main window
     win <- windowNew
     set win [ windowTitle := "Hello World" ]
     win `onDestroy` mainQuit

     -- create label and entry widgets
     grt <- labelNew Nothing
     nam <- entryNew
     
     -- create button
     hel <- buttonNew
     set hel [ buttonLabel := "Hello" ]
     hel `onClicked` hello nam grt
     
     -- describe layout
     lay <- vBoxNew False 5
     containerAdd lay grt
     containerAdd lay nam
     containerAdd lay hel
     --set win [ containerChild := lay ]

     containerAdd win lay     
     widgetShowAll win
     mainGUI

------------------------------------------------------------------------

hello :: Entry -> Label -> IO ()
hello nam grt =
  do s <- get nam entryText
     set grt [ labelText := ("Hello, " ++ s ++ "!") ]

------------------------------------------------------------------------

