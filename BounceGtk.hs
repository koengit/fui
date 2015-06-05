module Main where

import Graphics.UI.Gtk
import Data.IORef

------------------------------------------------------------------------

sizeX, sizeY, maxY, radius :: Int
sizeX  = 300
sizeY  = 400
maxY   = 400 - radius
radius = 20

------------------------------------------------------------------------

main :: IO ()
main =
  do initGUI

     -- create main window
     win <- windowNew
     windowSetTitle win "Bouncing Balls"
     win `onDestroy` mainQuit
     
     -- create abstract widget
     balls <- newIORef []
     
     -- create canvas (drawing area)
     can <- drawingAreaNew
     can `onSizeRequest` return (Requisition sizeX sizeY)
     can `onButtonPress` \_ -> withBallsEvent can balls (new can)
     can `onExpose`      \_ -> withBallsEvent can balls return
     
     -- create Clear button
     clr <- buttonNewWithLabel "Clear"
     clr `onClicked` withBalls can balls (\_ -> return [])

     -- create Close button     
     cls <- buttonNewWithLabel "Close"
     cls `onClicked` widgetDestroy win
     
     -- create timer; this runs the animation
     timeoutAdd (withBallsEvent can balls (\bs -> return (step bs))) 20
     
     -- describe layout of all widgets
     buts <- hBoxNew False 5
     containerAdd buts clr
     containerAdd buts cls
     
     lay <- vBoxNew False 5
     containerAdd lay can
     containerAdd lay buts
     
     containerAdd win lay
     widgetShowAll win
     mainGUI

------------------------------------------------------------------------

draw :: DrawingArea -> State -> IO ()
draw can bs =
  do dw <- widgetGetDrawWindow can
     drawWindowClear dw
     gc <- gcNew dw
     gcSetValues gc newGCValues{ foreground = red }
     sequence_ [ drawCircle dw gc p radius
               | p:_ <- bs
               ]
 where
  red = Color 65535 0 0 
 
  drawCircle dw gc (x,y) r =
    drawArc dw gc True (x-r) (y-r) (2*r) (2*r) 0 (360*64)

withBalls :: DrawingArea -> IORef State -> (State -> IO State) -> IO ()
withBalls can balls f =
  do bs <- readIORef balls
     bs' <- f bs
     writeIORef balls bs'
     draw can bs'

withBallsEvent :: DrawingArea -> IORef State -> (State -> IO State) -> IO Bool
withBallsEvent can balls f =
  do withBalls can balls f
     return True

new :: DrawingArea -> State -> IO State
new can bs =
  do p <- widgetGetPointer can
     return (bounce p 0 : bs)

------------------------------------------------------------------------

type State = [Ball]
type Ball  = [Point]

bounce :: Point -> Int -> Ball
bounce (x,y) v
  | v == 0 && y >= maxY = []
  | y' > maxY           = bounce (x,y) (2-v)
  | otherwise           = (x,y) : bounce (x,y') (v+1)
 where
  y' = y+v

step :: State -> State
step bs = [ ps' | p:ps' <- bs ]

------------------------------------------------------------------------

