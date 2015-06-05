module Main where

import Fui
import Control.Concurrent

main :: IO ()
main = run pills
{-
  do ch <- newChannel
     flush (source ch) stdout
     flush (source ch) stdout
     let loop = do s <- getLine
                   send (sink ch) s
                   loop
      in loop
-}

-- hello

hello0 :: GUI
hello0 =
  channel $ \name ->
    window [ text <+ val "Hello!" ] $
      label [ text <+ [ greet s | s <- source name ] ]
    .^^
      entry [ text +> sink name ]
 where
  greet s = "Hello, " ++ s ++ "!"

--

hello1 :: GUI
hello1 =
  channel $ \names ->
    window [ text <+ val "Hello!" ] $
      label  [ text <+ [ greet s | s <- name "" (source names) ] ]
    .^^
      entry  [ text     +> mapSink Left  (sink names)
             , activate +> mapSink Right (sink names)
             ]
    .^^
      button [ text     <+ val "Hi"
             , activate +> mapSink Right (sink names)
             ]
 where
  greet s = "Hello, " ++ s ++ "!"

  name s (Left s'  : ns) = name s' ns
  name s (Right () : ns) = s : name s ns

--

type SampleChannel a = Channel (Sample a)
data Sample a        = New a | Sample

samples :: a -> SampleChannel a -> [a]
samples x ch = samp x (source ch)
 where
  samp x (New x' : xs) = samp x' xs
  samp x (Sample : xs) = x : samp x xs

sample :: SampleChannel a -> Sink ()
sample ch = mapSink (const Sample) (sink ch)

new :: SampleChannel a -> Sink a
new ch = mapSink New (sink ch)

--

hello2 :: GUI
hello2 =
  channel $ \sname ->
    window [ text <+ val "Hello!" ] $
      label  [ text <+ [ greet s | s <- samples "" sname ] ]
    .^^
      label  [ text <+ [ greet s | s <- samples "" sname ] ]
    .^^
      entry  [ text     +> new sname
             , activate +> sample sname
             ]
    .^^
      button [ text     <+ val "Hi"
             , activate +> sample sname
             ]
 where
  greet s = "Hello, " ++ s ++ "!"

--

buttons :: GUI
buttons =
  channel $ \sname ->
    let number = show `fmap` count 1 [ () | Sample <- source sname ]

        names = accum [] (samples "" sname)

        buttons = fmap (\ns -> foldr (.^^) empty
                               [ button [ text     <+ val n
                                        , activate +> sample sname
                                        ]
                               | n <- ns
                               ]) names
     in
      window [ text <+ val "Click me" ]
      ( button [ text     <+ number
               , activate +> sample sname
               ]
    .^^ button [ text     <+ number
               , activate +> sample sname
               ]
    .^^ entry  [ text     <+ val ""
               , text     +> new sname
               , activate +> sample sname
               ]
      )
    .<<
      window [ text <+ fmap (++ " buttons!") number ]
      ( dynamic buttons
      )

count :: Int -> Source () -> Source Int
count n str = put n (get str (\_ str' -> count (n+1) str'))

accum :: [a] -> Source a -> Source [a]
accum ys xs = ys : case xs of x:xs' -> accum (x:ys) xs'

{-

mix :: [Source a] -> (Source a -> GUI) -> GUI
mix srcs k =
  channel $ \mx ->
    action $
      do sequence_ [ flush src mx | src <- srcs ]
         return (k (source mx))

when :: Source a -> Source () -> Source a
srcA `when` srcE = loop (error "when") (fmap Left srcA >< fmap Right srcE)
 where
  loop x src =
    get src $ \eae src' ->
      case eae of
        Left x'  -> loop x' src'
        Right () -> put x (loop x src')
-}

pills =
  channel $ \txt ->
    --window [ text <+ val "The Title" ] $
       entry [ text <+ source txt ]
     .^^
       (buttonColor txt "Red" .<< buttonColor txt "Blue")
 where
  buttonColor txt col =
    button [ text     <+ val col
           , activate +> mapSink (const col) (sink txt)
           ]

buttonSet =
  channel $ \stxt ->
    window [ text <+ val "Write Something" ] $
       entry [ text +> new stxt ]
     .^^
       (button [ text <+ val "Set!", activate +> sample stxt ]
     .<<
        button [ text <+ samples "" stxt ])

typedEntry =
  channel $ \str ->
  channel $ \prn ->
    window [ text <+ val "Write Something" ] $
       tentry [ value <+ val "apa"
              , value +> sink str
              , value +> new prn
              ]
     .^^
       button [ text     <+ source str
              , activate +> sample prn
              ]
     .^^
       (samples "" prn +>+ stdout)

