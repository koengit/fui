module Main where

import Fui
import Control.Exception

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
  channel $ \fileSelect ->
  let sts = foldSource clean (flip ($)) (source drive) in
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
             , activate +> const (file (\f -> const (load f) >!+ sink drive))
                        >+ sink fileSelect
             ]
  .<< button [ text     <+ val "Save"
             , activate +> const (file (\f -> const f >+ sink saves))
                        >+ sink fileSelect
             ]
    )
    .^^
      dynamic (source fileSelect)
    .^^
      ((sts `sync` source saves) +>+ save >+ stdio)

temp :: (Sink () -> GUI) -> GUI
temp f = channel $ \close -> dynamic (f (sink close) :> get (source close) (\_ _ -> val empty))

file :: (FilePath -> Sink ()) -> GUI
file snk =
  temp (\close ->
    foldr1 (.<<)
    [ button [ text <+ val f, activate +> (snk f & close) ]
    | f <- ["apa.txt", "bepa.txt", "cepa.txt"]
    ]
  )

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

load :: FilePath -> IO (State -> State)
load f =
  do ms <- try (readFile f)
     return (\st -> case ms of
                      Right s -> read s
                      _       -> st)

save :: (State, FilePath) -> IO ()
save (st,f) =
  do writeFile f (show st)

draw :: State -> Drawing
draw st = drawings $
  [ line p1 p2
  | (p1,p2) <- linez st
  ] ++
  [ Context (fgColor red) (line p1 p2)
  | Just (p1,p2) <- [current st]
  ] 
 where
  red   = rgb 65535 0 0

-------------------------------------------------------------------------

