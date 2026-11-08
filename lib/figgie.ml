type suit = Spades | Clubs | Hearts | Diamonds
type deck = { long : suit; short : suit }

let suits = [| Spades; Clubs; Hearts; Diamonds |]
let index = function Spades -> 0 | Clubs -> 1 | Hearts -> 2 | Diamonds -> 3

let partner = function
  | Spades -> Clubs
  | Clubs -> Spades
  | Hearts -> Diamonds
  | Diamonds -> Hearts

let letter = function
  | Spades -> 'S'
  | Clubs -> 'C'
  | Hearts -> 'H'
  | Diamonds -> 'D'

let decks =
  Array.to_list suits
  |> List.concat_map (fun long ->
      Array.to_list suits
      |> List.filter (( <> ) long)
      |> List.map (fun short -> { long; short }))
  |> Array.of_list

let counts d =
  Array.map
    (fun s -> if s = d.long then 12 else if s = d.short then 8 else 10)
    suits

let goal d = partner d.long
let pot = 200
let start_cash = 350
let ante players = pot / players
let bonus d = pot - (10 * (counts d).(index (goal d)))

let choose =
  let c = Array.make_matrix 41 41 0. in
  for n = 0 to 40 do
    c.(n).(0) <- 1.;
    for k = 1 to n do
      c.(n).(k) <- (c.(n - 1).(k - 1) +. if k < n then c.(n - 1).(k) else 0.)
    done
  done;
  fun n k -> if k < 0 || k > n then 0. else c.(n).(k)

let deal rng ~players =
  let d = decks.(Rng.int rng (Array.length decks)) in
  let cards =
    Array.concat
      (Array.to_list (Array.mapi (fun s n -> Array.make n s) (counts d)))
  in
  for i = Array.length cards - 1 downto 1 do
    let j = Rng.int rng (i + 1) in
    let x = cards.(i) in
    cards.(i) <- cards.(j);
    cards.(j) <- x
  done;
  let per = Array.length cards / players in
  let hands = Array.make_matrix players 4 0 in
  Array.iteri (fun i s -> hands.(i / per).(s) <- hands.(i / per).(s) + 1) cards;
  (d, hands)

let payouts d hands =
  let g = index (goal d) in
  let held = Array.map (fun h -> h.(g)) hands in
  let top = Array.fold_left max 0 held in
  let winners =
    Array.fold_left (fun n x -> if x = top then n + 1 else n) 0 held
  in
  Array.map
    (fun x ->
      float_of_int (10 * x)
      +. if x = top then float_of_int (bonus d) /. float_of_int winners else 0.)
    held

let posterior ?seen hand =
  let w =
    Array.map
      (fun d ->
        let n = counts d in
        let ok =
          match seen with
          | None -> true
          | Some seen -> Array.for_all2 ( <= ) seen n
        in
        if ok then Array.fold_left ( *. ) 1. (Array.map2 choose n hand) else 0.)
      decks
  in
  let total = Array.fold_left ( +. ) 0. w in
  Array.map (fun x -> x /. total) w

let goal_probs post =
  let p = Array.make 4 0. in
  Array.iteri
    (fun i d ->
      let g = index (goal d) in
      p.(g) <- p.(g) +. post.(i))
    decks;
  p

let odds x r others =
  let k = Array.length others in
  let rec go i r p top ties =
    if i = k - 1 then
      let p = p *. choose others.(i) r in
      let top, ties =
        if r > top then (r, 1)
        else if r = top then (top, ties + 1)
        else (top, ties)
      in
      if x > top then p
      else if x = top then p /. float_of_int (ties + 1)
      else 0.
    else
      let acc = ref 0. in
      for a = 0 to min r others.(i) do
        let top', ties' =
          if a > top then (a, 1)
          else if a = top then (top, ties + 1)
          else (top, ties)
        in
        acc := !acc +. go (i + 1) (r - a) (p *. choose others.(i) a) top' ties'
      done;
      !acc
  in
  go 0 r 1. (-1) 0 /. choose (Array.fold_left ( + ) 0 others) r

let memo = Domain.DLS.new_key (fun () -> Hashtbl.create 256)

let bonus_odds x r others =
  let sorted = Array.copy others in
  Array.sort compare sorted;
  let start = (((Array.length sorted * 64) + x) * 64) + r in
  let key = Array.fold_left (fun k n -> (k * 64) + n) start sorted in
  let tbl = Domain.DLS.get memo in
  match Hashtbl.find_opt tbl key with
  | Some p -> p
  | None ->
      let p = odds x r sorted in
      Hashtbl.add tbl key p;
      p

let share d mine others =
  let g = index (goal d) in
  let x = mine.(g) in
  let r = (counts d).(g) - x in
  if r < 0 || r > Array.fold_left ( + ) 0 others then 0.
  else float_of_int (10 * x) +. (float_of_int (bonus d) *. bonus_odds x r others)

let restrict post mine =
  let w =
    Array.mapi
      (fun i d ->
        if Array.for_all2 ( <= ) mine (counts d) then post.(i) else 0.)
      decks
  in
  let total = Array.fold_left ( +. ) 0. w in
  if total = 0. then None else Some (Array.map (fun x -> x /. total) w)

let mean_share post mine others =
  let acc = ref 0. in
  Array.iteri
    (fun i d ->
      if post.(i) > 0. then acc := !acc +. (post.(i) *. share d mine others))
    decks;
  !acc

let consistent post mine =
  match restrict post mine with
  | Some post -> post
  | None -> invalid_arg "Figgie: hand impossible under the posterior"

let expected_payout post mine others =
  mean_share (consistent post mine) mine others

let extreme pick others =
  let j = ref 0 in
  Array.iteri (fun i n -> if pick n others.(!j) then j := i) others;
  !j

let shift a i delta =
  let a = Array.copy a in
  a.(i) <- a.(i) + delta;
  a

let buy_value post mine others s =
  let s = index s in
  let more = shift mine s 1 in
  match restrict post more with
  | None -> 0.
  | Some post ->
      mean_share post more (shift others (extreme ( > ) others) (-1))
      -. mean_share post mine others

let sell_value post mine others s =
  let s = index s in
  let post = consistent post mine in
  mean_share post mine others
  -. mean_share post (shift mine s (-1)) (shift others (extreme ( < ) others) 1)

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

let create deck hands =
  let players = Array.length hands in
  {
    deck;
    hands = Array.map Array.copy hands;
    cash = Array.make players (start_cash - ante players);
    bids = Array.make 4 None;
    offers = Array.make 4 None;
    trades = [];
    tick = 0;
  }

let execute t suit px ~buyer ~seller =
  let i = index suit in
  t.hands.(seller).(i) <- t.hands.(seller).(i) - 1;
  t.hands.(buyer).(i) <- t.hands.(buyer).(i) + 1;
  t.cash.(seller) <- t.cash.(seller) + px;
  t.cash.(buyer) <- t.cash.(buyer) - px;
  Array.fill t.bids 0 4 None;
  Array.fill t.offers 0 4 None;
  let tr = { suit; px; buyer; seller; tick = t.tick } in
  t.trades <- tr :: t.trades;
  Traded tr

let act t p = function
  | Pass -> Rested
  | Bid (s, px) -> (
      let i = index s in
      let beaten =
        match t.bids.(i) with Some b -> px <= b.px | None -> false
      in
      if px < 1 || px > t.cash.(p) || beaten then Rejected
      else
        match t.offers.(i) with
        | Some o when px >= o.px ->
            if o.who = p then Rejected
            else execute t s o.px ~buyer:p ~seller:o.who
        | _ ->
            t.bids.(i) <- Some { px; who = p };
            Rested)
  | Offer (s, px) -> (
      let i = index s in
      let beaten =
        match t.offers.(i) with Some o -> px >= o.px | None -> false
      in
      if px < 1 || t.hands.(p).(i) = 0 || beaten then Rejected
      else
        match t.bids.(i) with
        | Some b when px <= b.px ->
            if b.who = p then Rejected
            else execute t s b.px ~buyer:b.who ~seller:p
        | _ ->
            t.offers.(i) <- Some { px; who = p };
            Rested)
  | Buy s -> (
      match t.offers.(index s) with
      | Some o when o.who <> p && o.px <= t.cash.(p) ->
          execute t s o.px ~buyer:p ~seller:o.who
      | _ -> Rejected)
  | Sell s -> (
      match t.bids.(index s) with
      | Some b when b.who <> p && t.hands.(p).(index s) > 0 ->
          execute t s b.px ~buyer:b.who ~seller:p
      | _ -> Rejected)

let pnl t =
  let pay = payouts t.deck t.hands in
  Array.mapi (fun p c -> float_of_int (c - start_cash) +. pay.(p)) t.cash

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

type tally = { seen : int array; known : int array array }

let tally ~players ~seat dealt =
  let known = Array.make_matrix players 4 0 in
  known.(seat) <- Array.copy dealt;
  { seen = Array.copy dealt; known }

let observe c (tr : trade) =
  let s = index tr.suit in
  if c.known.(tr.seller).(s) > 0 then
    c.known.(tr.seller).(s) <- c.known.(tr.seller).(s) - 1
  else c.seen.(s) <- c.seen.(s) + 1;
  c.known.(tr.buyer).(s) <- c.known.(tr.buyer).(s) + 1

let seen c = Array.copy c.seen

type bot = {
  name : string;
  make : Rng.t -> players:int -> seat:int -> int array -> view -> action;
}

let view (t : t) me ~ticks =
  {
    me;
    hand = Array.copy t.hands.(me);
    cash = t.cash.(me);
    sizes = Array.map (Array.fold_left ( + ) 0) t.hands;
    bids = Array.copy t.bids;
    offers = Array.copy t.offers;
    trades = t.trades;
    tick = t.tick;
    ticks;
  }

let play rng ?(ticks = 200) bots =
  let players = Array.length bots in
  let deck, hands = deal rng ~players in
  let t = create deck hands in
  let brains =
    Array.mapi
      (fun seat b ->
        b.make (Rng.split rng) ~players ~seat (Array.copy hands.(seat)))
      bots
  in
  let order = Array.init players Fun.id in
  for tick = 0 to ticks - 1 do
    t.tick <- tick;
    for i = players - 1 downto 1 do
      let j = Rng.int rng (i + 1) in
      let x = order.(i) in
      order.(i) <- order.(j);
      order.(j) <- x
    done;
    Array.iter (fun p -> ignore (act t p (brains.(p) (view t p ~ticks)))) order
  done;
  t
