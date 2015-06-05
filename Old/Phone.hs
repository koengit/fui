module Main where

import Prelude hiding (log)
import Fui

-- example

main :: IO ()
main = run phones

example :: GUI
example =
  channel $ \click ->
  channel $ \name ->
    let number :: Source String
        number = show `fmap` count 1 (source click)

        names :: Source [String]        
        names = accum (put "" (source name) `when` put () (source click))

        buttons :: Source GUI
        buttons = fmap (\ns -> foldr1 (.^^)
                               [ button [ text <+ val n
                                        , activate +> sink click
                                        ]
                               | n <- ns
                               ]) names
     in
      window [ text <+ val "Click me" ]
      ( button [ text <+ number
               , activate +> sink click
               ]
    .^^ entry [ text +> sink name, activate +> sink click ]
      )
    .<<
      window [ text <+ fmap (++ " buttons!") number ]
      ( dynamic buttons
      )

count :: Int -> Source () -> Source Int
count n str = put n (get str (\_ str' -> count (n+1) str'))

accum :: Source a -> Source [a]
accum src = loop [] src
 where
  loop xs src =
    get src $ \x src' ->
      let xs' = x:xs in
        put (reverse xs') (loop xs' src')

when :: Source a -> Source () -> Source a
srcA `when` srcE = loop (error "when") (fmap Left srcA >< fmap Right srcE)
 where
  loop x src =
    get src $ \eae src' ->
      case eae of
        Left x'  -> loop x' src'
        Right () -> put x (loop x src')

example2 =
  channel $ \txt ->
    window [ text <+ val "The Title" ] $
       entry [ text <+ source txt ]
     .^^
       (button [ text <+ val "red", activate +> sinkMap (const "RED") (sink txt) ]
     .<<
        button [ text <+ val "blue", activate +> sinkMap (const "BLUE") (sink txt) ])

example3 =
  channel $ \txt ->
  channel $ \click ->
    window [ text <+ val "Write Something" ] $
       entry [ text +> sink txt ]
     .^^
       (button [ text <+ val "Set!", activate +> sink click ]
     .<<
        button [ text <+ (source txt `when` source click) ])

data State = Idle | TryCall | CallIncoming | Connected deriving Show
data Action = Dial | PickUp | HangUp deriving Show

phones :: GUI
phones = channel $ \phone1 ->
         channel $ \phone2 ->
           phone phone1 (source phone2) .<<
           phone phone2 (source phone1) .<<
           log (fmap ((++) "Phone 1: " . show) (source phone1) ><
                fmap ((++) "Phone 2: " . show) (source phone2))

log :: Source String -> GUI
log strs = window [ text <+ val "Log" ] (dynamic (fmap (buttonWithText . unlines) (accum strs)))
  where buttonWithText xs = button [ text <+ val xs ]

phone :: Channel Action -> Source Action -> GUI
phone thePhone neighbour =
    let gui :: State -> GUI
        gui Idle = button [ text <+ val "Call", activate +> perform Dial ]
        gui TryCall = button [ text <+ val "Abandon call", activate +> perform HangUp ]
        gui CallIncoming = button [ text <+ val "Accept call", activate +> perform PickUp ] .^^
                           button [ text <+ val "Reject call", activate +> perform HangUp ]
        gui Connected = button [ text <+ val "Hang up", activate +> perform HangUp ]
        perform action = sinkMap (const action) (sink thePhone)

        -- Alter the state depending on what this phone does.
        sending Dial = TryCall
        sending PickUp = Connected
        sending HangUp = Idle

        -- Alter the state depending on what the other phone does.
        receiving Dial = CallIncoming
        receiving PickUp = Connected
        receiving HangUp = Idle

        state = put Idle (fmap receiving neighbour >< fmap sending (source thePhone))
      in
        window [ text <+ fmap show state ] (dynamic (fmap gui state))

