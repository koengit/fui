module Main where

import Graphics.UI.WX
import Data.IORef

-------------------------------------------------------------------------

main :: IO ()
main =
  do initGUI
     win <- windowNewWithTitle "Drawing Editor"
     
     state <- newIORef initState
     
     can <- drawingAreaNew
     can `onSizeRequest` return (Requisition 300 300)
     can `onExpose` \_ -> paintState can state

     let update f =
           do st <- readIORef state
              writeIORef state (f st)
              paintState pan state

     can `onButtonClick`   \_ -> update clickState
     can `onButtonRelease` \_ -> update clickState
     can `onButtonRelease` \_ -> update clickState


     set pan [ on click   := update . clickState
	     , on unclick := update . unclickState
	     , on drag    := update . dragState
             ]
     
     undo <- button frm [ text       := "Undo"
                        , on command := update undoState
                        ]
     load <- button frm [ text       := "Load" ]
     save <- button frm [ text       := "Save" ]
     
     let lay = column 5 [ minsize (sz 300 300) (widget pan)
                        , floatCenter (row 5 [ widget undo
                                             , widget load
                                             , widget save ])
                        ]
     set frm [ layout := lay ]

-------------------------------------------------------------------------

data State =
  State
  { linez :: [(Point,Point)]
  , currentLine :: Maybe (Point,Point)
  }
 deriving (Read, Show)

-------------------------------------------------------------------------

{-
This function is a GUI-dependent State-function.
-}

paintState :: Var State -> DC a -> area -> IO ()
paintState state dc area =
  do st <- get state value
     sequence_
       [ line dc p0 p1 [penColor := black]
       | (p0,p1) <- linez st
       ]
     case currentLine st of
       Nothing -> return ()
       Just (p0,p1) -> line dc p0 p1 [penColor := red]

-------------------------------------------------------------------------

{-
Note how these functions are completely independent of what happens in
the GUI.
-}

initState :: State
initState = State{ linez = [], currentLine = Nothing }

clickState, unclickState, dragState :: Point -> State -> State
clickState pt st =
  st{ currentLine = Just (pt,pt) }

unclickState pt (State ls (Just (pt0,_))) =
  State{ linez = (pt0,pt):ls, currentLine = Nothing }
unclickState pt st = st

dragState pt st@(State _ (Just (pt0,_))) =
  st{ currentLine = Just (pt0,pt) }
dragState pt st = st

undoState :: State -> State
undoState st = st{ linez = drop 1 (linez st) }

-------------------------------------------------------------------------
