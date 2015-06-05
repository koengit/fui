
data Layout
  = Layout :<< Layout
  | Layout :^^ Layout
  | Justify Justify Layout
  | Widget Widget
  | Empty

a :<< Empty = a
Empty :<< b = b

a :^^ Empty = a
Empty :^^ b = b

Justify [] a = a
Justify j Empty = Empty
Justify j1 (Justify j2 a) = Justify (j1++j2) a

a :<< (b :<< c) = (a :<< b) :<< c
a :^^ (b :^^ c) = (a :^^ b) :^^ c

Justify j (a :<< b) = Justify (filter verti j) (Justify (filter horiz j) a :<< Justify (filter horiz j) b)
Justify j (a :^^ b) = Justify (filter horiz j) (Justify (filter verti j) a :<< Justify (filter verti j) b)

--

data Layout
  = Horiz [Layout]
  | Verti [Layout]
  | Justify [Justify]
  | Widget Widget
  | Empty

(.<) :: Layout -> Layout -> Layout
Empty    .< q        = q
p        .< Empty    = p
Horiz ps .< Horiz qs = Horiz (ps ++ qs)
Horiz ps .< q        = Horiz (ps ++ [q])
p        .< Horiz qs = Horiz (p:qs)
p        .< q        = Horiz [p,q]

(.^) :: Layout -> Layout -> Layout
Empty    .^ q        = q
p        .^ Empty    = p
Verti ps .^ Verti qs = Verti (ps ++ qs)
Verti ps .^ q        = Verti (ps ++ [q])
p        .^ Verti qs = Verti (p:qs)
p        .^ q        = Verti [p,q]

justify :: [Justify] -> Layout -> Layout
justify js Empty      = Empty
justify js (Horiz ps) = Justify js1 (Horiz (map (justify js2) ps))
 where
  (js1,js2) = partition isVertiJust js
justify js (Verti ps) = Justify js1 (Verti (map (justify js2) ps))
 where
  (js1,js2) = partition isVertiJust js
  


