module Main where

import Graphics.UI.Gtk

main :: IO ()
main =
  do initGUI
     
     win <- windowNew
     set win [ windowTitle := "Hello!" ]
     win `onDestroy` mainQuit
     
     grt <- labelNew Nothing
     nam <- entryNew
     hel <- buttonNew
     set hel [ buttonLabel := "Hello" ]
     hel `onClicked` hello nam grt
     
     vbx <- vBoxNew False 5
     containerAdd vbx grt
     containerAdd vbx nam
     containerAdd vbx hel
     
     set win [ containerChild := vbx ]
     widgetShowAll win
     mainGUI
  
hello :: Entry -> Label -> IO ()
hello nam grt =
  do s <- get nam entryText
     set grt [ labelText := ("Hello, " ++ s ++ "!") ]
  
