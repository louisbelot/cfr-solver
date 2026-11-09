type relation = Le | Eq | Ge
type row = { coeffs : (int * Rat.t) list; rel : relation; rhs : Rat.t }

type result =
  | Optimal of { value : Rat.t; x : Rat.t array; pivots : int }
  | Infeasible
  | Unbounded

module type FIELD = sig
  type t

  val zero : t
  val one : t
  val of_rat : Rat.t -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val sign : t -> int
  val compare : t -> t -> int
end

module Exact : FIELD with type t = Rat.t
module Approx : FIELD with type t = float

val maximize :
  ?bland:bool ->
  ?warm:bool ->
  vars:int ->
  (int * Rat.t) list ->
  row list ->
  result

val maximize_float :
  ?bland:bool ->
  vars:int ->
  (int * Rat.t) list ->
  row list ->
  (float * float array) option
