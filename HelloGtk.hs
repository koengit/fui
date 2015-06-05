module Main where

import Graphics.UI.Gtk

------------------------------------------------------------------------

main :: IO ()
main =
  do initGUI
     
     -- create main window
     win <- windowNew
     windowSetTitle win "Hello World"
     win `onDestroy` mainQuit

     -- create label and entry widgets
     grt <- labelNew Nothing
     nam <- entryNew
     
     -- create button
     hel <- buttonNewWithLabel "Hello"
     hel `onClicked` hello nam grt
     
     -- describe layout
     lay <- vBoxNew False 5
     containerAdd lay grt
     containerAdd lay nam
     containerAdd lay hel

     containerAdd win lay     
     widgetShowAll win
     mainGUI

------------------------------------------------------------------------

hello :: Entry -> Label -> IO ()
hello nam grt =
  do s <- entryGetText nam
     labelSetText grt ("Hello, " ++ s ++ "!")

------------------------------------------------------------------------

