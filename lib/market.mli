type config = { cards : int; hidden : int; width : int; rounds : int }
type side = Buy | Sell | Pass

val name : config -> string
val mids : config -> int * int
val fair : config -> int -> float
val pnl : config -> side -> int -> int -> float

type history

module Make (_ : sig
  val config : config
end) : Game.S with type history = history

val game : config -> (module Game.S)
val quotes : config -> (int -> int) -> Strategy.t
val revealing : config -> Strategy.t
val uninformed : config -> Strategy.t
