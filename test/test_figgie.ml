open Cfr_solver
open Figgie

let near ?(eps = 1e-12) msg a b = Alcotest.(check (float eps)) msg a b
let deck long short = { long; short }
let one_hot d = Array.map (fun d' -> if d' = d then 1. else 0.) decks

let structure () =
  Alcotest.(check int) "twelve decks" 12 (Array.length decks);
  Array.iter
    (fun d ->
      Alcotest.(check int) "40 cards" 40 (Array.fold_left ( + ) 0 (counts d));
      Alcotest.(check bool)
        "same colour" true
        (index (goal d) / 2 = index d.long / 2 && goal d <> d.long))
    decks;
  let eight = Array.to_list decks |> List.filter (fun d -> bonus d = 120) in
  Alcotest.(check int) "goal of 8 in a third of decks" 4 (List.length eight);
  Alcotest.(check int)
    "otherwise 100" 8
    (Array.to_list decks |> List.filter (fun d -> bonus d = 100) |> List.length)

let ten_spades () =
  let g = goal_probs (posterior [| 10; 0; 0; 0 |]) in
  near "clubs" (198. /. 204.) g.(index Clubs);
  List.iter
    (fun s -> near "rest" (2. /. 204.) g.(index s))
    [ Spades; Hearts; Diamonds ]

let uniform () =
  Array.iter (near "flat" (1. /. 12.)) (posterior [| 0; 0; 0; 0 |]);
  Array.iter (near "quarter" 0.25) (goal_probs (posterior [| 0; 0; 0; 0 |]))

let seen_bound () =
  let g = goal_probs (posterior ~seen:[| 11; 0; 0; 0 |] [| 5; 2; 2; 1 |]) in
  near "eleven spades seen" 1. g.(index Clubs);
  let free = posterior [| 5; 2; 2; 1 |]
  and loose = posterior ~seen:[| 5; 2; 2; 1 |] [| 5; 2; 2; 1 |] in
  Array.iteri (fun i p -> near "hand alone binds nothing" p loose.(i)) free

let calibration () =
  let rng = Rng.make 7 in
  let n = 40_000 in
  let hits = Array.make 11 0.
  and mass = Array.make 11 0.
  and seen = Array.make 11 0 in
  for _ = 1 to n do
    let d, hands = deal rng ~players:4 in
    let k = hands.(0).(0) in
    seen.(k) <- seen.(k) + 1;
    mass.(k) <- mass.(k) +. (goal_probs (posterior hands.(0))).(index Clubs);
    if goal d = Clubs then hits.(k) <- hits.(k) +. 1.
  done;
  for k = 0 to 10 do
    if seen.(k) >= 500 then begin
      let m = float_of_int seen.(k) in
      let p = mass.(k) /. m and f = hits.(k) /. m in
      let se = sqrt (Float.max (p *. (1. -. p)) 1e-4 /. m) in
      if Float.abs (p -. f) > 4. *. se then
        Alcotest.failf "%d spades: posterior %.4f, frequency %.4f" k p f
    end
  done

let site_example () =
  let d = deck Spades Clubs in
  let hands =
    [| [| 0; 3; 0; 0 |]; [| 0; 3; 0; 0 |]; [| 0; 2; 0; 0 |]; [| 0; 0; 0; 0 |] |]
  in
  Alcotest.(check (array (float 1e-12)))
    "split 120" [| 90.; 90.; 20.; 0. |] (payouts d hands)

let pot_paid () =
  let rng = Rng.make 3 in
  for players = 4 to 5 do
    for _ = 1 to 200 do
      let d, hands = deal rng ~players in
      Array.iter
        (fun h ->
          Alcotest.(check int)
            "hand size" (40 / players)
            (Array.fold_left ( + ) 0 h))
        hands;
      near "pot" 200. (Array.fold_left ( +. ) 0. (payouts d hands))
    done
  done

let locked_bonus () =
  let d = deck Hearts Spades in
  let p = one_hot d in
  let others = [| 10; 10; 10 |] in
  near "six of ten" 160. (expected_payout p [| 2; 2; 0; 6 |] others);
  let mine = [| 1; 2; 0; 7 |] in
  near "buy goal" 10. (buy_value p mine others Diamonds);
  near "sell goal" 10. (sell_value p mine others Diamonds);
  near "buy other" 0. (buy_value p mine others Clubs);
  let ten = [| 10; 0; 0; 0 |] in
  near "eleventh spade proves spades long" 0.
    (buy_value (posterior ten) ten others Spades);
  Alcotest.check_raises "impossible hand"
    (Invalid_argument "Figgie: hand impossible under the posterior") (fun () ->
      ignore (expected_payout p [| 0; 0; 0; 11 |] others))

let payout_mc () =
  let d = deck Hearts Spades in
  let mine = [| 2; 3; 2; 3 |] in
  let exact = expected_payout (one_hot d) mine [| 10; 10; 10 |] in
  let rest =
    Array.to_list (counts d)
    |> List.mapi (fun s n -> List.init (n - mine.(s)) (fun _ -> s))
    |> List.concat |> Array.of_list
  in
  let rng = Rng.make 11 and n = 50_000 in
  let sum = ref 0. and sq = ref 0. in
  for _ = 1 to n do
    for i = Array.length rest - 1 downto 1 do
      let j = Rng.int rng (i + 1) in
      let x = rest.(i) in
      rest.(i) <- rest.(j);
      rest.(j) <- x
    done;
    let hands =
      Array.init 4 (fun p -> if p = 0 then Array.copy mine else Array.make 4 0)
    in
    Array.iteri
      (fun i s -> hands.(1 + (i / 10)).(s) <- hands.(1 + (i / 10)).(s) + 1)
      rest;
    let x = (payouts d hands).(0) in
    sum := !sum +. x;
    sq := !sq +. (x *. x)
  done;
  let m = !sum /. float_of_int n in
  let se = sqrt (((!sq /. float_of_int n) -. (m *. m)) /. float_of_int n) in
  if Float.abs (m -. exact) > 4. *. se then
    Alcotest.failf "exact %.4f, simulated %.4f +- %.4f" exact m se

let tower () =
  let rng = Rng.make 5 in
  List.iter
    (fun players ->
      let n = 20_000 in
      let sum = ref 0. and sq = ref 0. in
      for _ = 1 to n do
        let _, hands = deal rng ~players in
        let others = Array.make (players - 1) (40 / players) in
        let x = expected_payout (posterior hands.(0)) hands.(0) others in
        sum := !sum +. x;
        sq := !sq +. (x *. x)
      done;
      let m = !sum /. float_of_int n in
      let se = sqrt (((!sq /. float_of_int n) -. (m *. m)) /. float_of_int n) in
      let fair = float_of_int (pot / players) in
      if Float.abs (m -. fair) > 4. *. se then
        Alcotest.failf "%d players: %.4f +- %.4f, fair %.1f" players m se fair)
    [ 4; 5 ]

let outcome =
  Alcotest.testable
    (fun f -> function
      | Rested -> Format.fprintf f "rested"
      | Rejected -> Format.fprintf f "rejected"
      | Traded tr -> Format.fprintf f "traded at %d" tr.px)
    ( = )

let fresh () =
  let hands = Array.init 4 (fun _ -> [| 3; 3; 2; 2 |]) in
  create (deck Spades Hearts) hands

let book () =
  let t = fresh () in
  Alcotest.(check int) "after ante" 300 t.cash.(0);
  let ok msg o a = Alcotest.check outcome msg o a in
  ok "bid" Rested (act t 0 (Bid (Spades, 5)));
  ok "same price" Rejected (act t 1 (Bid (Spades, 5)));
  ok "improve" Rested (act t 1 (Bid (Spades, 6)));
  ok "own offer through own bid" Rejected (act t 1 (Offer (Spades, 6)));
  ok "offer" Rested (act t 2 (Offer (Hearts, 9)));
  ok "worse offer" Rejected (act t 3 (Offer (Hearts, 9)));
  ok "buy own" Rejected (act t 2 (Buy Hearts));
  ok "zero" Rejected (act t 3 (Bid (Clubs, 0)));
  t.cash.(3) <- 8;
  ok "too poor" Rejected (act t 3 (Buy Hearts));
  ok "over cash" Rejected (act t 3 (Bid (Diamonds, 9)));
  t.cash.(3) <- 300;
  (match act t 3 (Bid (Hearts, 12)) with
  | Traded tr ->
      Alcotest.(check int) "at resting offer" 9 tr.px;
      Alcotest.(check (pair int int)) "sides" (3, 2) (tr.buyer, tr.seller)
  | _ -> Alcotest.fail "crossing bid should trade");
  Alcotest.(check (array int)) "cash" [| 300; 300; 309; 291 |] t.cash;
  Alcotest.(check int) "card moved" 3 t.hands.(3).(index Hearts);
  Alcotest.(check bool)
    "all quotes cleared" true
    (Array.for_all Option.is_none t.bids
    && Array.for_all Option.is_none t.offers);
  t.hands.(0).(index Clubs) <- 0;
  ok "no card" Rejected (act t 0 (Offer (Clubs, 3)));
  ok "bid again" Rested (act t 1 (Bid (Clubs, 4)));
  ok "sell without card" Rejected (act t 0 (Sell Clubs));
  match act t 2 (Offer (Clubs, 2)) with
  | Traded tr -> Alcotest.(check int) "at resting bid" 4 tr.px
  | _ -> Alcotest.fail "crossing offer should trade"

let lineup = Array.of_list Figgie_bots.all

let invariants () =
  QCheck2.Test.check_exn
  @@ QCheck2.Test.make ~count:40 ~name:"figgie conserves cards and cash"
       QCheck2.Gen.(int_bound 1_000_000)
       (fun seed ->
         let t = play (Rng.make seed) ~ticks:120 lineup in
         let players = Array.length lineup in
         let n = counts t.deck in
         let held s = Array.fold_left (fun a h -> a + h.(s)) 0 t.hands in
         Array.for_all (fun h -> Array.for_all (fun x -> x >= 0) h) t.hands
         && List.for_all (fun s -> held s = n.(s)) [ 0; 1; 2; 3 ]
         && Array.for_all (fun c -> c >= 0) t.cash
         && Array.fold_left ( + ) 0 t.cash = (start_cash * players) - pot
         && Float.abs (Array.fold_left ( +. ) 0. (pnl t)) < 1e-9
         && List.for_all
              (fun (tr : trade) -> tr.px >= 1 && tr.buyer <> tr.seller)
              t.trades)

let tally_bound () =
  QCheck2.Test.check_exn
  @@ QCheck2.Test.make ~count:40 ~name:"card counting never overcounts"
       QCheck2.Gen.(int_bound 1_000_000)
       (fun seed ->
         let rng = Rng.make seed in
         let d, hands = deal (Rng.copy rng) ~players:5 in
         let t = play rng ~ticks:150 lineup in
         let n = counts d in
         t.deck = d
         && List.for_all
              (fun seat ->
                let c = tally ~players:5 ~seat hands.(seat) in
                List.iter (observe c) (List.rev t.trades);
                let s = seen c in
                Array.for_all2 ( <= ) s n
                && Array.for_all2 ( <= ) hands.(seat) s)
              [ 0; 1; 2; 3; 4 ])

let fundamentalists_win () =
  let bots = Figgie_bots.[| fundamentalist (); noise; noise; noise |] in
  let r = Figgie_bots.tournament (Rng.make 1) ~games:300 bots in
  near ~eps:1e-9 "zero sum" 0.
    (Array.fold_left (fun a s -> a +. s.Figgie_bots.mean) 0. r);
  Alcotest.(check bool)
    "fundamentalist ahead" true
    (r.(0).mean > 4. *. r.(0).stderr)

let tests =
  [
    Alcotest.test_case "decks" `Quick structure;
    Alcotest.test_case "ten spades" `Quick ten_spades;
    Alcotest.test_case "empty hand" `Quick uniform;
    Alcotest.test_case "seen cards" `Quick seen_bound;
    Alcotest.test_case "posterior calibrated" `Quick calibration;
    Alcotest.test_case "tie split" `Quick site_example;
    Alcotest.test_case "pot paid out" `Quick pot_paid;
    Alcotest.test_case "locked bonus" `Quick locked_bonus;
    Alcotest.test_case "payout simulated" `Quick payout_mc;
    Alcotest.test_case "fair share" `Quick tower;
    Alcotest.test_case "order book" `Quick book;
    Alcotest.test_case "conservation" `Quick invariants;
    Alcotest.test_case "tally bound" `Quick tally_bound;
    Alcotest.test_case "fundamentalist edge" `Quick fundamentalists_win;
  ]
