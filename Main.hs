module Main where

----------------------------------------------------------------------------

import Graphics.UI.Fui

----------------------------------------------------------------------------

main :: IO ()
main = run hello1

----------------------------------------------------------------------------

hello :: GUI
hello =
  channel $ \name ->
    window [ text <+ val "Hello!" ] $
      label [ text <+ (greet `fmap` source name) ]
    .^^
      entry [ text +> sink name ]
 where
  greet s = "Hello, " ++ s ++ "!"

----------------------------------------------------------------------------

hello1 :: GUI
hello1 =
  channel $ \name ->
  channel $ \click ->
    window [ text <+ val "Hello!" ] $
      label [ text <+ (greet `fmap` (source name `when` source click)) ]
    .^^
      entry [ text +> sink name ]
    .^^
      button [ text <+ val "Hej", activate +> sink click ]
 where
  greet s = "Hello, " ++ s ++ "!"

----------------------------------------------------------------------------

