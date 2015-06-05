module Main where

import Graphics.UI.WX
import Data.IORef

{-
In this file, the Load and Save buttons do work.
Please compare this file with Draw0.hs, where Load and Save do not work!
-}

-------------------------------------------------------------------------

main :: IO ()
main = start draw

draw :: IO ()
draw =
  do frm <- frame [ text := "Drawing Editor" ]
     
     state <- variable  [ value := initState ]
     pan   <- panel frm [ on paint := paintState state ]

     let updateIO f =
           do st <- get state value
              st' <- f st
              set state [ value := st' ]
              repaint pan

         update f =
           updateIO (return . f)

     set pan [ on click   := update . clickState
	     , on unclick := update . unclickState
	     , on drag    := update . dragState
             ]
     
     undo <- button frm [ text       := "Undo"
                        , on command := update undoState
                        ]
     load <- button frm [ text       := "Load"
                        , on command := updateIO (loadState frm)
                        ]
     save <- button frm [ text       := "Save"
                        , on command := updateIO (saveState frm)
                        ]
     
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
These functions are GUI-dependent State-functions.
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

loadState :: Frame () -> State -> IO State
loadState frm =
  readState (fileOpenDialog frm True True "Load" formats "" deflt)

saveState :: Frame () -> State -> IO State
saveState frm =
  writeState (fileSaveDialog frm True True "Save" formats "" deflt)

formats = [("Drawings (*.draw)",["*.draw"])]
deflt   = "Untitled.draw"

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

{-
These functions are in IO, but still GUI-independent.
-}

readState :: IO (Maybe FilePath) -> State -> IO State
readState getFile st =
  do mfile <- getFile
     case mfile of
       Nothing   -> do return st
       Just file -> do s <- readFile file
		       return (read s)

writeState :: IO (Maybe FilePath) -> State -> IO State
writeState getFile st =
  do mfile <- getFile
     case mfile of
       Nothing   -> do return st
       Just file -> do writeFile file (show st)
		       return st

-------------------------------------------------------------------------
