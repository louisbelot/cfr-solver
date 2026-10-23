module Make
    (Full : Game.S)
    (Abs : Game.S with type history = Full.history) : sig
  val map : (string * string) list
  val size : int
  val lift : Strategy.t -> Strategy.t
end
