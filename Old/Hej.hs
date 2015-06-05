module Main where

import Fui

main = run hello

hello =
    channel $ \name ->
    channel $ \clk ->
      let greeting = fmap ("Hello " ++) (source name `when` source clk) in
    
      window [ text <+ val "Apa"  ]
      ( label [ text <+ greeting ]
    .^^ entry [ text +> sink name ]
    .^^ button [ text <+ val "Hello", activate +> sink clk ]
      )

