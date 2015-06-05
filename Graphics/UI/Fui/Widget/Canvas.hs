module Graphics.UI.Fui.Widget.Canvas where

----------------------------------------------------------------------------

import Graphics.UI.Fui.Stream
import Graphics.UI.Fui.Event
import Graphics.UI.Fui.Sink
import Graphics.UI.Fui.GUI

----------------------------------------------------------------------------

import qualified Graphics.UI.Gtk as Gtk
import Data.IORef

----------------------------------------------------------------------------

type Point = (Int,Int)

data Canvas
  = MkCanvas Gtk.DrawingArea (Event Gtk.DrawWindow) (Channel Drawing) (Channel Point) (Channel Point) (Channel Point)

canvas :: [Prop Canvas] -> GUI
canvas props =
  MkGUI $
  do can <- Gtk.drawingAreaNew
     chaDraw      <- newChannel
     getLastDraw  <- mkGetLast chaDraw (drawings [])
     chaClick     <- newChannel
     chaRel       <- newChannel
     chaMouse     <- newChannel
     (evtDrw,drw) <- newEvent
     can `Gtk.onExpose` \_ ->
       do dw <- Gtk.widgetGetDrawWindow can
          send drw dw
          dr <- getLastDraw
          drawDrawing dw dr
          return True
     let w = MkCanvas can evtDrw chaDraw chaClick chaRel chaMouse
     sequence_ [ prop w | MkProp prop <- props ]
     return (Widget (Gtk.toWidget can))
 where
  mkGetLast ch x =
    do ref <- newIORef x
       source ch `flush` writeIORef ref
       return (readIORef ref)

instance Sized Canvas where
  size (x,y) =
    MkProp (\(MkCanvas w _ _ _ _ _) ->
      do w `Gtk.onSizeRequest` return (Gtk.Requisition x y)
         return ()
    )

--

sizeDr :: Drawing -> Int
sizeDr (Context _ d) = sizeDr d
sizeDr (Drawings ds) = sum (map sizeDr ds)
sizeDr _             = 1

data Drawing
  = Line Point Point
  | Slice Bool Point (Int,Int) (Int,Int)
  | Context Context Drawing
  | Drawings [Drawing]

line :: Point -> Point -> Drawing
line p1 p2 = Line p1 p2

lines :: [Point] -> Drawing
lines ps = drawings [ line p1 p2 | (p1,p2) <- ps `zip` drop 1 ps ]

circle :: Bool -> Point -> Int -> Drawing
circle f p r = Slice f p (r,r) (0,64*360)

drawings :: [Drawing] -> Drawing
drawings ds = Drawings ds

type Context = Gtk.GCValues -> Gtk.GCValues
type Color   = Gtk.Color

fgColor, bgColor :: Color -> Context
fgColor c gv = gv{ Gtk.foreground = c }
bgColor c gv = gv{ Gtk.background = c }

rgb = Gtk.Color

context :: Context -> Drawing -> Drawing
context ctx d = Context ctx d

drawing :: Attr Canvas Drawing
drawing = MkAttr
  (\(MkCanvas can drwEvt chaDraw chaClick chaRel chaMouse) snk ->
    do flush (source chaDraw) snk
  )
  (\(MkCanvas can drwEvt chaDraw chaClick chaRel chaMouse) src ->
    do flush src $ \drw ->
         drwEvt `triggers` \dw ->
           do drawDrawing dw drw
       flush src (sink chaDraw)
  )

drawDrawing :: Gtk.DrawWindow -> Drawing -> IO ()
drawDrawing dw d =
  do Gtk.drawWindowClear dw
     gc <- Gtk.gcNew dw
     let draw (Line p1 p2)                    = Gtk.drawLine dw gc p1 p2
         draw (Slice f (x,y) (rx,ry) (d1,d2)) = Gtk.drawArc dw gc f (x-rx) (y-ry) (2*rx) (2*ry) d1 d2
         draw (Drawings ds)                   = sequence_ [ draw d | d <- ds ]
         draw (Context k d)                   = do vs <- Gtk.gcGetValues gc
                                                   Gtk.gcSetValues gc (k vs)
                                                   draw d
                                                   Gtk.gcSetValues gc vs
      in draw d

mouseClick :: Attr Canvas Point
mouseClick = MkAttr
  (\(MkCanvas can _ chaDraw chaClick chaRel chaMouse) snk ->
    do can `Gtk.onButtonPress` \_ ->
         do p <- Gtk.widgetGetPointer can
            send snk p
            return True
       flush (source chaClick) snk
  )
  (\(MkCanvas can _ chaDraw chaClick chaRel chaMouse) src ->
    do flush src (sink chaClick)
  )

mouseRelease :: Attr Canvas Point
mouseRelease = MkAttr
  (\(MkCanvas can _ chaDraw chaClick chaRel chaMouse) snk ->
    do can `Gtk.onButtonRelease` \_ ->
         do p <- Gtk.widgetGetPointer can
            send snk p
            return True
       flush (source chaRel) snk
  )
  (\(MkCanvas can _ chaDraw chaClick chaRel chaMouse) src ->
    do flush src (sink chaRel)
  )

mouse :: Attr Canvas Point
mouse = MkAttr
  (\(MkCanvas can _ chaDraw chaClick chaRel chaMouse) snk ->
    do Gtk.onMotionNotify can True $ \_ ->
         do p <- Gtk.widgetGetPointer can
            send snk p
            return True
       flush (source chaMouse) snk
  )
  (\(MkCanvas can _ chaDraw chaClick chaRel chaMouse) src ->
    do flush src (sink chaMouse)
  )

----------------------------------------------------------------------------

