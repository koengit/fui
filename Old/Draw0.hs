module Main where

import Graphics.UI.Fui

-------------------------------------------------------------------------

sizeX, sizeY :: Int
sizeX  = 400
sizeY  = 400

-------------------------------------------------------------------------

main :: IO ()
main = run drawEdit

-------------------------------------------------------------------------

drawEdit :: GUI
drawEdit =
  channel $ \drive ->
  channel $ \saves ->
    draw' drive saves .<< draw' drive saves

draw' drive saves =
  let sts = fold ($) clean (source drive) in
  window [ text <+ val "Drawing Editor" ] $
      canvas [ size (sizeX,sizeY)
             , drawing      <+ draw    +< sts
             , mouseClick   +> click   >+ sink drive
             , mouseRelease +> release >+ sink drive
             , mouse        +> move    >+ sink drive
             ]
    .^^
    ( button [ text     <+ val "Clear"
             , activate +> const clear >+ sink drive
             ]
  .<< button [ text     <+ val "Undo"
             , activate +> const undo  >+ sink drive
             ]
  .<< button [ text     <+ val "Load"
             , activate +> const load >!+ sink drive
             ]
  .<< button [ text     <+ val "Save"
             , activate +> sink saves
             ]
    )
    .^^
      ((sts `when` source saves) +>+ save >+ stdio)

-------------------------------------------------------------------------

data State
  = State
  { linez   :: [(Point,Point)]
  , current :: Maybe (Point,Point)
  }
 deriving ( Eq, Show, Read )

clean :: State
clean = State{ linez = [], current = Nothing }

click :: Point -> State -> State
click p st = st{ current = Just (p,p) }

move :: Point -> State -> State
move p st = case current st of
              Nothing     -> st
              Just (p1,_) -> st{ current = Just (p1,p) }

release :: Point -> State -> State
release p st = case current st of
                 Nothing     -> st
                 Just (p1,_) -> st{ linez   = (p1,p) : linez st
                                  , current = Nothing
                                  }

clear :: State -> State
clear st = clean

undo :: State -> State
undo st = st{ linez = drop 1 (linez st) }

load :: IO (State -> State)
load =
  do ms <- try (readFile "drawing.txt")
     return (\st -> case ms of
                      Right s -> read s
                      _       -> st)

save :: State -> IO ()
save st =
  do writeFile "drawing.txt" (show st)

draw :: State -> Drawing
draw st = drawings $
  [ Line p1 p2
  | (p1,p2) <- linez st
  ] ++
  [ Context (fgColor red) (Line p1 p2)
  | Just (p1,p2) <- [current st]
  ] 
 where
  red   = rgb 65535 0 0

-------------------------------------------------------------------------

