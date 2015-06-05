module Graphics.UI.Fui.Widget.Connect where

----------------------------------------------------------------------------

import Graphics.UI.Fui.Stream
import Graphics.UI.Fui.Sink
import Graphics.UI.Fui.GUI

----------------------------------------------------------------------------

infix 3 +>+

----------------------------------------------------------------------------

(+>+) :: Stream a -> Sink a -> GUI
str +>+ snk = action (do flush str snk; return empty)

----------------------------------------------------------------------------

