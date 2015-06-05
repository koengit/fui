module Graphics.UI.Fui.Widget.Entry where

----------------------------------------------------------------------------

import Graphics.UI.Fui.Stream
import Graphics.UI.Fui.Sink
import Graphics.UI.Fui.GUI

----------------------------------------------------------------------------

import qualified Graphics.UI.Gtk as Gtk

----------------------------------------------------------------------------

data Entry
  = MkEntry Gtk.Entry (Channel ()) (Channel String)

entry :: [Prop Entry] -> GUI
entry props =
  MkGUI $
  do ent <- Gtk.entryNew
     chaAct  <- newChannel
     chaText <- newChannel
     let w = MkEntry ent chaAct chaText
     sequence_ [ prop w | MkProp prop <- props ]
     return (Widget (Gtk.toWidget ent))

instance Sized Entry where
  size (x,y) =
    MkProp (\(MkEntry w _ _) ->
      do w `Gtk.onSizeRequest` return (Gtk.Requisition x y)
         return ()
    )

instance Active Entry where
  activate = MkAttr
    (\(MkEntry ent chaAct chaText) snk ->
      do ent `Gtk.onEntryActivate` send snk ()
         flush (source chaAct) snk
    )
    (\(MkEntry ent chaAct chaText) src ->
      do flush src (sink chaAct)
    )

instance Text Entry where
  text = MkAttr
    (\(MkEntry ent chaAct chaText) snk ->
      do ent `Gtk.onKeyRelease` \_ ->
           do s <- Gtk.entryGetText ent
              send snk s
              return False
         flush (source chaText) snk
    )
    (\(MkEntry ent chaAct chaText) src ->
      do flush src (Gtk.entrySetText ent)
         flush src (sink chaText)
    )

----------------------------------------------------------------------------

data TEntry a
  = MkTEntry Gtk.Entry (Channel ()) (Channel a) (a -> String) (String -> Maybe a)

tentry :: (Show a, Read a) => [Prop (TEntry a)] -> GUI
tentry props =
  MkGUI $
  do ent <- Gtk.entryNew
     chaAct <- newChannel
     chaVal <- newChannel
     let w = MkTEntry ent chaAct chaVal show readMaybe
     sequence_ [ prop w | MkProp prop <- props ]
     return (Widget (Gtk.toWidget ent))
 where
  readMaybe s = case reads s of
                  (x,""):_ -> Just x
                  _        -> Nothing

instance Sized (TEntry a) where
  size (x,y) =
    MkProp (\(MkTEntry w _ _ _ _) ->
      do w `Gtk.onSizeRequest` return (Gtk.Requisition x y)
         return ()
    )

instance Active (TEntry a) where
  activate = MkAttr
    (\(MkTEntry ent chaAct chaVal show read) snk ->
      do ent `Gtk.onEntryActivate` send snk ()
         flush (source chaAct) snk
    )
    (\(MkTEntry ent chaAct chaVal show read) src ->
      do flush src (sink chaAct)
    )

instance Container TEntry where
  value = MkAttr
    (\(MkTEntry ent chaAct chaVal show read) snk ->
      do ent `Gtk.onKeyRelease` \_ ->
           do s <- Gtk.entryGetText ent
              case read s of
                Just x  -> send snk x
                Nothing -> return ()
              return False
         flush (source chaVal) snk
    )
    (\(MkTEntry ent chaAct chaVal show read) src ->
      do flush src (Gtk.entrySetText ent . show)
         flush src (sink chaVal)
    )

----------------------------------------------------------------------------

