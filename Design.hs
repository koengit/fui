
type Event a

data Stream a
  = Wait (Event (Flow a))

data Flow a
  = Emit a (Stream a)
