module Main where

import Graphics.UI.Gtk
import Expr

------------------------------------------------------------------------
-- F.

points :: Expr -> Double -> (Int,Int) -> [Point]
points e s (wd,hg) =
  [ (x, realToPix (eval e (pixToReal x)))
  | x <- [0..wd]
  ]
 where
  pixToReal :: Int -> Double
  pixToReal px = fromIntegral (px - (wd `div` 2)) * s
  
  realToPix :: Double -> Int
  realToPix ry = (hg `div` 2) - round (ry / s)
 
------------------------------------------------------------------------
-- G.

linez :: Expr -> Double -> (Int,Int) -> [(Point,Point)]
linez e s (wd,hg) = pts `zip` tail pts
 where
  pts = points e s (wd,hg)
  
------------------------------------------------------------------------
-- H.

scale :: Double
scale = 0.02

sizeX, sizeY :: Int
sizeX = 300
sizeY = 300

main :: IO ()
main =
  do initGUI

     -- create window
     win <- windowNew
     windowSetTitle win "Function Graph"
     
     -- entry
     ent <- entryNew
     
     -- canvas
     can <- drawingAreaNew
     can `onSizeRequest` return (Requisition sizeX sizeY)

     -- linking canvas and entry
     can `onExpose` updateCanvas can ent
     ent `onEntryActivate` drawCanvas can ent
     
     -- layout
     lay <- vBoxNew False 5
     containerAdd lay can
     containerAdd lay ent
     --set win [ containerChild := lay ]
     containerAdd win lay

     -- finishing up
     onDestroy win mainQuit
     widgetShowAll win
     mainGUI

updateCanvas :: DrawingArea -> Entry -> event -> IO Bool
updateCanvas can ent _evt =
  do drawCanvas can ent
     return True

drawCanvas :: DrawingArea -> Entry -> IO ()
drawCanvas can ent =
  do win <- widgetGetDrawWindow can
     drawWindowClear win
     s   <- entryGetText ent
     gc  <- gcNew win
     case readExpr s of
       Just e  -> sequence_ [ drawLine win gc p1 p2
                            | (p1,p2) <- linez e scale (sizeX,sizeY)
                            ]
       Nothing -> return ()

{-
main :: IO ()
main = start functionFrame

functionFrame :: IO ()
functionFrame =
  do frm <- frame     [ text     := "Function Graph" ]
     ent <- entry frm [ text     := "x"  ]
     pan <- panel frm [ on paint := draw ent ]
     set ent [ on command := repaint pan ]
     set frm [ layout := column 5
                         [ minsize (sz sizeX sizeY) (widget pan)
                         , floatCenter (widget ent)
                         ]
             ]

draw :: TextCtrl () -> DC a -> rect -> IO ()
draw ent dc area =
  do s <- get ent text
     case readExpr s of
       Just e  -> sequence_ [ line dc p1 p2 [] | (p1,p2) <- linez e scale (sizeX,sizeY) ]
       Nothing -> return ()
-}

------------------------------------------------------------------------
