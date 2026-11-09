module Make
    (F : Lp.FIELD)
    (_ : sig
      val delta : F.t
    end) : sig
  type t

  val create : shift:F.t -> t
  val rows : t -> int
  val cols : t -> int
  val add_row : t -> F.t array -> unit
  val add_col : t -> F.t array -> unit
  val solution : t -> F.t array * F.t array * F.t
  val gap : t -> F.t array * F.t array -> F.t
  val refactor : t -> unit
  val pivots : t -> int
  val refactors : t -> int
end
