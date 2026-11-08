val response :
  offset:int array ->
  parent:int array ->
  order:int array ->
  eta:float ->
  float array ->
  float array

module Make (G : Game.S) : sig
  type t

  val create : ?optimistic:bool -> eta:float -> unit -> t
  val iterate : t -> unit
  val run : t -> int -> unit
  val iterations : t -> int
  val touched : t -> int
  val average_strategy : t -> Strategy.t
  val current_strategy : t -> Strategy.t
end
