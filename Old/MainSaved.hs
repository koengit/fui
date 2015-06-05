module Main where

import Fui

main :: IO ()
main = run hello

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

samples :: SampleChannel a -> [a]
samples ch = samp (error "no initial sample") (source ch)
 where
  samp x (New x' : xs) = samp x' ns
  samp x (Sample : xs) = x : samp x ns

sample :: SampleChannel a -> Sink ()
sample ch = mapSink (const Sample) ch

new :: SampleChannel a -> Sink a
new ch = mapSink New ch

--

hello2 :: GUI
hello2 =
  channel $ \names ->
    window [ text <+ val "Hello!" ] $
      label  [ text <+ [ greet s | s <- name "" (samples names) ] ]
    .^^
      entry  [ text     +> new names
             , activate +> sample names
             ]
    .^^
      button [ text     <+ val "Hi"
             , activate +> sample names
             ]
 where
  greet s = "Hello, " ++ s ++ "!"

--



{-
hello :: GUI
hello =
  channel $ \click ->
  channel $ \name ->
    window [ text <+ val "Hello!" ] $
      label  [ text     <+ fmap greet (put "" (source name) `when` source click)
             ]
    .^^
      entry  [ text     +> sink name
             , activate +> sink click
             ]
    .^^
      button [ text     <+ val "Hello"
             , activate +> sink click
             ]
 where
  greet s = "Hello, " ++ s ++ "!"
-}


example :: GUI
example =
  channel $ \names ->
    let number :: Source String
        number = show `fmap` count 1 (source click)

        names :: Source [String]        
        names = accum (source name `when` source click)

        buttons :: Source GUI
        buttons = fmap (\ns -> foldr (.^^) empty
                               [ button [ text     <+ val n
                                        , activate +> sink click
                                        ]
                               | n <- ns
                               ]) names
     in
      window [ text <+ val "Click me" ]
      ( button [ text     <+ number
               , activate +> sink click
               ]
    .^^ entry  [ text     <+ val ""
               , text     +> sink name
               , activate +> sink click
               ]
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

mix :: [Source a] -> (Source a -> GUI) -> GUI
mix srcs k =
  channel $ \mx ->
    action $
      do sequence_ [ flush src mx | src <- srcs ]
         return (k (source mx))

{-
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
       (button [ text <+ val "red", activate +> mapSink (const "RED") (sink txt) ]
     .<<
        button [ text <+ val "blue", activate +> mapSink (const "BLUE") (sink txt) ])

example3 =
  channel $ \txt ->
  channel $ \click ->
    window [ text <+ val "Write Something" ] $
       entry [ text +> sink txt ]
     .^^
       (button [ text <+ val "Set!", activate +> sink click ]
     .<<
        button [ text <+ (source txt `when` source click) ])

example5 =
  channel $ \click ->
    window [ text <+ val "Click It!" ] $
       (button [ text <+ val "Click?", activate <+ source click ]
     .<<
        button [ text <+ val "Click!", activate +> sink click ])

example4 =
  channel $ \pair ->
    window [ text <+ val "Write Something" ] $
       tentry [ value +> sink (pair :: Channel (Int,Bool)), value <+ val (17,True) ]
     .^^
       button [ text <+ fmap show (source pair) ]
-}

