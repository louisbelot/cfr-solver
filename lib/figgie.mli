type suit = Spades | Clubs | Hearts | Diamonds
type deck = { long : suit; short : suit }

val suits : suit array
val index : suit -> int
val partner : suit -> suit
val letter : suit -> char
val decks : deck array
val counts : deck -> int array
val goal : deck -> suit
val bonus : deck -> int
val pot : int
val start_cash : int
val ante : int -> int
val deal : Rng.t -> players:int -> deck * int array array
val payouts : deck -> int array array -> float array
val posterior : ?seen:int array -> int array -> float array
val goal_probs : float array -> float array
val expected_payout : float array -> int array -> int array -> float
val buy_value : float array -> int array -> int array -> suit -> float
val sell_value : float array -> int array -> int array -> suit -> float

type quote = { px : int; who : int }
type trade = { suit : suit; px : int; buyer : int; seller : int; tick : int }

type action =
  | Pass
  | Bid of suit * int
  | Offer of suit * int
  | Buy of suit
  | Sell of suit

type outcome = Rested | Traded of trade | Rejected

type t = {
  deck : deck;
  hands : int array array;
  cash : int array;
  bids : quote option array;
  offers : quote option array;
  mutable trades : trade list;
  mutable tick : int;
}

val create : deck -> int array array -> t
val act : t -> int -> action -> outcome
val pnl : t -> float array

type view = {
  me : int;
  hand : int array;
  cash : int;
  sizes : int array;
  bids : quote option array;
  offers : quote option array;
  trades : trade list;
  tick : int;
  ticks : int;
}

type tally

val tally : players:int -> seat:int -> int array -> tally
val observe : tally -> trade -> unit
val seen : tally -> int array

type bot = {
  name : string;
  make : Rng.t -> players:int -> seat:int -> int array -> view -> action;
}

val view : t -> int -> ticks:int -> view
val play : Rng.t -> ?ticks:int -> bot array -> t
