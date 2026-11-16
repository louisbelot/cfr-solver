type spot = { key : string; labels : string array; moves : Figgie.action array }

val spot :
  dealt:int array ->
  seen:int array ->
  hand:int array ->
  others:int array ->
  bids:Figgie.quote option array ->
  offers:Figgie.quote option array ->
  me:int ->
  phase:int ->
  spot

val seen_cards :
  players:int -> seat:int -> int array -> Figgie.trade list -> int array

type pace = Clock | Events
type state

module type CONFIG = sig
  val players : int
  val rounds : int
  val pace : pace
end

module Make (_ : CONFIG) : Lazy_mccfr.GAME with type state = state

module type FIELD = sig
  include CONFIG

  val field : Figgie.bot array
end

module Against (_ : FIELD) : Lazy_mccfr.GAME with type state = state

val bot :
  name:string -> pace:pace -> rounds:int -> Lazy_mccfr.policy -> Figgie.bot
