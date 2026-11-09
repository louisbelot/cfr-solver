type rollout = Own | Uniform | Prefer of string | Fixed of Strategy.t

val rollout_name : rollout -> string

val respond :
  ?rollout:rollout -> Multi_tree.t -> int -> float array -> float * float array

val gains : ?rollout:rollout -> Multi_tree.t -> Strategy.t -> float array
val nash_conv : ?rollout:rollout -> Multi_tree.t -> Strategy.t -> float

module Make (G : Multi.S) : sig
  val value : ?rollout:rollout -> int -> Strategy.t -> float
  val strategy : ?rollout:rollout -> int -> Strategy.t -> Strategy.t
  val gains : ?rollout:rollout -> Strategy.t -> float array
  val nash_conv : ?rollout:rollout -> Strategy.t -> float
end
