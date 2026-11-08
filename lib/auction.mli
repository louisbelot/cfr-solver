type rule = First_price | Second_price | All_pay
type ties = Split | Void

type config = {
  bidders : int;
  top : int;
  rule : rule;
  ties : ties;
  reserve : int;
}

val rule_name : rule -> string
val name : config -> string
val key : int -> int -> string

module type FIELD = sig
  type t

  val zero : t
  val one : t
  val of_int : int -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val compare : t -> t -> int
end

module type EVAL = sig
  type num
  type profile = num array array array

  val pure : config -> int array array -> profile
  val symmetric : config -> int array -> profile
  val outcome : config -> num array array -> num array * num array
  val utilities : config -> profile -> int -> num array array
  val value : config -> profile -> int -> num
  val best : config -> profile -> int -> num * int array
  val gains : config -> profile -> num array
  val nash_conv : config -> profile -> num
  val revenue : config -> profile -> num
  val is_equilibrium : config -> profile -> bool
  val symmetric_pure_equilibria : config -> int array list
end

module Make (F : FIELD) : EVAL with type num = F.t
module Approx : EVAL with type num = float
module Exact : EVAL with type num = Rat.t

val rationalize :
  ?max_den:int -> float array array array -> Rat.t array array array

val strategy : config -> float array array array -> Strategy.t
val profile : config -> Strategy.t -> float array array array

type history

val game : config -> (module Multi.S with type history = history)

module Solver : sig
  type t

  val create : ?algorithm:Cfr.algorithm -> ?symmetric:bool -> config -> t
  val iterate : t -> unit
  val run : t -> int -> unit
  val iterations : t -> int
  val current : t -> float array array array
  val average : t -> float array array array
end

module Theory : sig
  val bid : rule -> bidders:int -> ?reserve:float -> float -> float
  val revenue : bidders:int -> ?reserve:float -> unit -> float
end
