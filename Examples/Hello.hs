module Main where

----------------------------------------------------------------------------

import Graphics.UI.Fui

----------------------------------------------------------------------------

main :: IO ()
main = run hello

----------------------------------------------------------------------------

hello :: GUI
hello =
 channel $ \name ->
 channel $ \click ->
   window [ text <+ val "Hej" ]
   ( label  [ text     <+ put "" (greet `fmap` source name) `when` source click ]
 .^^ entry  [ text     +> sink name ]
 .^^ button [ text     <+ val "Hello"
            , activate +> sink click
            ]
   )
 where
  greet s = "Hello, " ++ s ++ "!"

----------------------------------------------------------------------------
