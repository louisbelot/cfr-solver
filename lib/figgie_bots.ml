open Figgie

let passive =
  { name = "passive"; make = (fun _ ~players:_ ~seat:_ _ _ -> Pass) }

let noise =
  let make rng ~players:_ ~seat:_ _ (v : view) =
    if Rng.int rng 10 < 7 then Pass
    else
      let i = Rng.int rng 4 and px = 1 + Rng.int rng 15 in
      let s = suits.(i) in
      match Rng.int rng 4 with
      | 0 -> Bid (s, px)
      | 1 -> Offer (s, px)
      | 2 -> (
          match v.offers.(i) with Some o when o.px <= px -> Buy s | _ -> Pass)
      | _ -> (
          match v.bids.(i) with Some b when b.px >= px -> Sell s | _ -> Pass)
  in
  { name = "noise"; make }

let others (v : view) =
  Array.of_list (List.filteri (fun p _ -> p <> v.me) (Array.to_list v.sizes))

let decide rng (v : view) ~edge buy sell =
  let best = ref (0.5, Pass) in
  let consider gain a = if gain > fst !best then best := (gain, a) in
  Array.iteri
    (fun i s ->
      (match v.offers.(i) with
      | Some o when o.who <> v.me && o.px <= v.cash ->
          consider (buy.(i) -. float_of_int o.px) (Buy s)
      | _ -> ());
      match v.bids.(i) with
      | Some b when b.who <> v.me && v.hand.(i) > 0 ->
          consider (float_of_int b.px -. sell.(i)) (Sell s)
      | _ -> ())
    suits;
  match snd !best with
  | Pass ->
      let i = Rng.int rng 4 in
      if Rng.int rng 2 = 0 then
        let px = int_of_float (Float.floor (buy.(i) -. edge)) in
        if px >= 1 then Bid (suits.(i), px) else Pass
      else if v.hand.(i) > 0 then
        Offer (suits.(i), max 1 (int_of_float (Float.ceil (sell.(i) +. edge))))
      else Pass
  | a -> a

let valuer name ~counting ~edge =
  let make rng ~players ~seat dealt =
    let c = tally ~players ~seat dealt in
    let post = ref (posterior dealt) and processed = ref 0 in
    let cached = ref (-1)
    and buy = Array.make 4 0.
    and sell = Array.make 4 0. in
    fun (v : view) ->
      let n = List.length v.trades in
      if n <> !cached then begin
        if counting then begin
          List.filteri (fun i _ -> i < n - !processed) v.trades
          |> List.rev
          |> List.iter (observe c);
          processed := n;
          post := posterior ~seen:(seen c) dealt
        end;
        let rest = others v in
        Array.iteri
          (fun i s ->
            buy.(i) <- buy_value !post v.hand rest s;
            sell.(i) <-
              (if v.hand.(i) > 0 then sell_value !post v.hand rest s else 0.))
          suits;
        cached := n
      end;
      decide rng v ~edge buy sell
  in
  { name; make }

let fundamentalist ?(edge = 2.) () =
  valuer "fundamentalist" ~counting:false ~edge

let counter ?(edge = 2.) () = valuer "counter" ~counting:true ~edge

let bottom_feeder =
  let make rng ~players:_ ~seat:_ _ (v : view) =
    let i = Rng.int rng 4 in
    let s = suits.(i) in
    let prices =
      List.filter_map
        (fun (tr : trade) ->
          if tr.suit = s then Some (float_of_int tr.px) else None)
        v.trades
    in
    let theirs = function Some q when q.who <> v.me -> Some q | _ -> None in
    let bid = theirs v.bids.(i) and offer = theirs v.offers.(i) in
    let estimate =
      match (prices, bid, offer) with
      | _ :: _, _, _ ->
          Some
            (List.fold_left ( +. ) 0. prices
            /. float_of_int (List.length prices))
      | [], Some b, Some o -> Some (float_of_int (b.px + o.px) /. 2.)
      | _ -> None
    in
    match estimate with
    | None -> Pass
    | Some e -> (
        match (bid, offer) with
        | _, Some o when float_of_int o.px < e -. 1. && o.px <= v.cash -> Buy s
        | Some b, _ when float_of_int b.px > e +. 1. && v.hand.(i) > 0 -> Sell s
        | _ ->
            if Rng.int rng 2 = 0 then
              let px =
                match bid with Some b -> b.px + 1 | None -> int_of_float e - 3
              in
              if px >= 1 && float_of_int px < e -. 1. then Bid (s, px) else Pass
            else
              let px =
                match offer with
                | Some o -> o.px - 1
                | None -> int_of_float e + 3
              in
              if v.hand.(i) > 0 && float_of_int px > e +. 1. then Offer (s, px)
              else Pass)
  in
  { name = "bottom-feeder"; make }

let all = [ passive; noise; fundamentalist (); counter (); bottom_feeder ]

let find spec =
  match String.split_on_char ':' spec with
  | [ name ] -> List.find_opt (fun b -> b.name = name) all
  | [ "fundamentalist"; e ] ->
      Option.map (fun edge -> fundamentalist ~edge ()) (float_of_string_opt e)
  | [ "counter"; e ] ->
      Option.map (fun edge -> counter ~edge ()) (float_of_string_opt e)
  | _ -> None

type stats = { mean : float; stderr : float }

let tournament rng ~games ?ticks bots =
  let k = Array.length bots in
  let sum = Array.make k 0. and sq = Array.make k 0. in
  let seats = Array.init k Fun.id in
  for _ = 1 to games do
    for i = k - 1 downto 1 do
      let j = Rng.int rng (i + 1) in
      let x = seats.(i) in
      seats.(i) <- seats.(j);
      seats.(j) <- x
    done;
    let t = play rng ?ticks (Array.map (fun b -> bots.(b)) seats) in
    Array.iteri
      (fun seat x ->
        let b = seats.(seat) in
        sum.(b) <- sum.(b) +. x;
        sq.(b) <- sq.(b) +. (x *. x))
      (pnl t)
  done;
  let n = float_of_int games in
  Array.init k (fun b ->
      let mean = sum.(b) /. n in
      let var =
        Float.max 0. ((sq.(b) /. n) -. (mean *. mean)) *. n /. (n -. 1.)
      in
      { mean; stderr = sqrt (var /. n) })
