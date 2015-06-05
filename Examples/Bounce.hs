module Main where

import Graphics.UI.Fui

-------------------------------------------------------------------------

sizeX, sizeY, maxY, radius :: Int
sizeX  = 300
sizeY  = 400
maxY   = 400 - radius
radius = 20

-------------------------------------------------------------------------

main :: IO ()
main = run bouncing

-------------------------------------------------------------------------

bouncing :: GUI
bouncing =
  channel $ \drive ->
  channel $ \new -> doit drive new .<< doit drive new

doit drive new =
    window [ text <+ val "Bouncing" ] (
      canvas [ size (sizeX,sizeY)
             , drawing    <+ animation (source drive)
             , mouseClick +> mapSink click (sink drive)
             ]
    .^^
    ( button [ text     <+ val "Clear"
             , activate +> mapSink (const clear) (sink drive)
             ]
    .<<
      button [ text     <+ val "New"
             , activate +> sink new
             ]
    )
    .^^
      timer 20 [ activate +> mapSink (const tick) (sink drive) ]
  )
  .^^
  newWindows (source new)

newWindows :: Stream () -> GUI
newWindows ticks = dynamic (get ticks $ \() ticks' -> put (bouncing .^^ newWindows ticks') nil)

-------------------------------------------------------------------------

type Ball = (Point,Int)
type Anim = [Ball] -> [Ball]

animation :: Stream Anim -> Stream Drawing
animation = fmap draw . fold ($) start

start :: [Ball]
start = []

click :: Point -> [Ball] -> [Ball]
click p bs = bs ++ [(p,0)]

tick :: [Ball] -> [Ball]
tick bs = [ b' | b <- bs, Just b' <- [step b] ]

clear :: [Ball] -> [Ball]
clear bs = []

draw :: [Ball] -> Drawing
draw bs = drawings $ concat
  [ [ Context (fgColor red) (circle True p radius)
    , circle False p radius
    ]
  | (p,v) <- bs
  ]
 where
  red   = rgb 65535 0 0
  black = rgb 0 0 0

step :: Ball -> Maybe Ball
step ((x,y),v)
  | v == 0 && y >= maxY = Nothing
  | y' > maxY           = Just ((x,y),2-v)
  | otherwise           = Just ((x,y'),v+1)
 where
  y' = y+v

-------------------------------------------------------------------------

