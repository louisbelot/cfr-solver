open Figgie

type spot = { key : string; labels : string array; moves : action array }

let bucket p =
  if p < 0.5 then 0 else if p < 0.75 then 1 else if p < 0.95 then 2 else 3

let argmax a =
  let j = ref 0 in
  Array.iteri (fun i x -> if x > a.(!j) then j := i) a;
  !j

let price f v = max 1 (int_of_float (Float.round (f *. v)))

type values = { g : int; conf : int; bv : float; sv : float array }

let evaluate dealt seen hand others =
  let post = posterior ~seen dealt in
  let probs = goal_probs post in
  let g = argmax probs in
  {
    g;
    conf = bucket probs.(g);
    bv = buy_value post hand others suits.(g);
    sv =
      Array.mapi
        (fun i n -> if n > 0 then sell_value post hand others suits.(i) else 0.)
        hand;
  }

let cache = Domain.DLS.new_key (fun () -> Hashtbl.create 4096)

let values ~dealt ~seen ~hand ~others =
  let key =
    String.init
      (12 + Array.length others)
      (fun i ->
        Char.chr
          (if i < 4 then dealt.(i)
           else if i < 8 then seen.(i - 4)
           else if i < 12 then hand.(i - 8)
           else others.(i - 12)))
  in
  let tbl = Domain.DLS.get cache in
  match Hashtbl.find_opt tbl key with
  | Some v -> v
  | None ->
      let v = evaluate dealt seen hand others in
      if Hashtbl.length tbl >= 1 lsl 20 then Hashtbl.reset tbl;
      Hashtbl.add tbl key v;
      v

let spot ~dealt ~seen ~hand ~others ~bids ~offers ~me ~phase =
  let { g; conf; bv; sv } = values ~dealt ~seen ~hand ~others in
  let s = suits.(g) and held = hand.(g) in
  let sv_g = sv.(g) in
  let bid =
    match bids.(g) with
    | None -> 0
    | Some q when q.who = me -> 1
    | Some q -> if held > 0 && float_of_int q.px >= sv_g then 3 else 2
  in
  let offer =
    match offers.(g) with
    | None -> 0
    | Some q when q.who = me -> 1
    | Some q -> if float_of_int q.px <= bv then 3 else 2
  in
  let junk = ref None in
  Array.iteri
    (fun i q ->
      match q with
      | Some (q : quote) when i <> g && q.who <> me && hand.(i) > 0 -> (
          let edge = float_of_int q.px -. sv.(i) in
          match !junk with
          | _ when edge <= 0. -> ()
          | Some (_, e) when e >= edge -> ()
          | _ -> junk := Some (suits.(i), edge))
      | _ -> ())
    bids;
  let moves =
    [
      ("pass", Pass);
      ("bid-lo", Bid (s, price 0.6 bv));
      ("bid-hi", Bid (s, price 0.9 bv));
    ]
    @ (if held > 0 then
         [
           ("offer-lo", Offer (s, price 1.1 sv_g));
           ("offer-hi", Offer (s, price 1.4 sv_g));
         ]
       else [])
    @ (if offer >= 2 then [ ("lift", Buy s) ] else [])
    @ (if bid >= 2 && held > 0 then [ ("hit", Sell s) ] else [])
    @ match !junk with Some (j, _) -> [ ("dump", Sell j) ] | None -> []
  in
  {
    key =
      Printf.sprintf "%d%d%d%d%d%d" phase conf (min held 5) bid offer
        (if Option.is_some !junk then 1 else 0);
    labels = Array.of_list (List.map fst moves);
    moves = Array.of_list (List.map snd moves);
  }

let seen_cards ~players ~seat dealt trades =
  let c = tally ~players ~seat dealt in
  List.iter (observe c) (List.rev trades);
  seen c

let others sizes me =
  Array.of_list (List.filteri (fun p _ -> p <> me) (Array.to_list sizes))

type state =
  | Deal
  | Between of { g : t; dealt : int array array; round : int }
  | Turn of {
      g : t;
      dealt : int array array;
      round : int;
      order : int array;
      pos : int;
    }

module type CONFIG = sig
  val players : int
  val rounds : int
end

module Make (C : CONFIG) = struct
  type nonrec state = state

  let () =
    if C.players < 4 || C.players > 5 then
      invalid_arg "Figgie_abs: 4 or 5 players";
    if C.rounds < 1 then invalid_arg "Figgie_abs: rounds must be positive"

  let name = Printf.sprintf "figgie-%dp-%dr" C.players C.rounds
  let players = C.players
  let root = Deal

  let shuffle rng =
    let order = Array.init players Fun.id in
    for i = players - 1 downto 1 do
      let j = Rng.int rng (i + 1) in
      let x = order.(i) in
      order.(i) <- order.(j);
      order.(j) <- x
    done;
    order

  let node = function
    | Deal ->
        Lazy_mccfr.Chance
          (fun rng ->
            let deck, hands = deal rng ~players in
            Between { g = create deck hands; dealt = hands; round = 0 })
    | Between { g; round; _ } when round = C.rounds -> Terminal (pnl g)
    | Between { g; dealt; round } ->
        Chance
          (fun rng -> Turn { g; dealt; round; order = shuffle rng; pos = 0 })
    | Turn { g; dealt; round; order; pos } ->
        let p = order.(pos) in
        let sizes = Array.map (Array.fold_left ( + ) 0) g.hands in
        let sp =
          spot ~dealt:dealt.(p)
            ~seen:(seen_cards ~players ~seat:p dealt.(p) g.trades)
            ~hand:g.hands.(p) ~others:(others sizes p) ~bids:g.bids
            ~offers:g.offers ~me:p ~phase:round
        in
        let next a =
          let g = copy g in
          g.tick <- round;
          ignore (act g p sp.moves.(a));
          if pos + 1 = players then Between { g; dealt; round = round + 1 }
          else Turn { g; dealt; round; order; pos = pos + 1 }
        in
        Decision { player = p; infoset = sp.key; actions = sp.labels; next }
end

let bot ~name ~rounds policy =
  let make rng ~players ~seat dealt =
    let c = tally ~players ~seat dealt in
    let processed = ref 0 in
    fun (v : view) ->
      let n = List.length v.trades in
      if n <> !processed then begin
        List.filteri (fun i _ -> i < n - !processed) v.trades
        |> List.rev
        |> List.iter (observe c);
        processed := n
      end;
      let sp =
        spot ~dealt ~seen:(seen c) ~hand:v.hand ~others:(others v.sizes v.me)
          ~bids:v.bids ~offers:v.offers ~me:v.me
          ~phase:(min (rounds - 1) (v.tick * rounds / v.ticks))
      in
      let k = Array.length sp.labels in
      sp.moves.(Rng.pick rng (policy sp.key k) 0 k)
  in
  { name; make }
