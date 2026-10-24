open Cfr_solver

let river = Bet_tree.limit ~pot:4. ~bet:1. 1
let turn = Bet_tree.limit ~cap:2 ~pot:4. ~bet:1. 2
let river_board = Hand.parse "Ah Kd 7c 7s 2h"
let turn_board = Hand.parse "Ah Kd 7c 2h"
let r0 = Range.of_string "AA, KK, AK, 77:0.5, A7s, QJs, 22, T9s:0.25"
let r1 = Range.of_string "AK, KQ, 77, A2s, JTs, 33:0.75"
let small0 = Range.of_string "AA:0.9, AKs:0.7, 77:0.55, QJs"
let small1 = Range.of_string "KK:0.85, AQs, 72s:0.65, T9s:0.45"

let close tol name expected got =
  if Float.abs (expected -. got) > tol then
    Alcotest.failf "%s: expected %.12g, got %.12g" name expected got

let same_strategy name ref_strategy got =
  List.iter
    (fun (key, (e : Strategy.entry)) ->
      match Strategy.find got key with
      | None -> Alcotest.failf "%s: no %s" name key
      | Some g ->
          Array.iteri
            (fun a p ->
              close 1e-9 (Printf.sprintf "%s %s %d" name key a) p g.probs.(a))
            e.probs)
    (Strategy.bindings ref_strategy)

let matches_scalar_cfr cfg board r0 r1 algorithm iterations () =
  let (module G) = Subgame_ref.make cfg ~board r0 r1 in
  let module C = Cfr.Make (G) in
  let c = C.create ~algorithm () in
  C.run c iterations;
  let t = Subgame.create ~algorithm cfg ~board r0 r1 in
  Subgame.run t iterations;
  same_strategy
    (Cfr.algorithm_name algorithm)
    (C.average_strategy c)
    (Subgame.strategy t (Subgame.average t))

let matches_scalar_values cfg board r0 r1 () =
  let (module G) = Subgame_ref.make cfg ~board r0 r1 in
  let module B = Best_response.Make (G) in
  let module E = Eval.Make (G) in
  let t = Subgame.create ~algorithm:Cfr.Plus cfg ~board r0 r1 in
  List.iter
    (fun n ->
      Subgame.run t n;
      let prof = Subgame.average t in
      let s = Subgame.strategy t prof in
      close 1e-9 "value" (E.expected_value s) (Subgame.value t prof);
      close 1e-9 "br0" (B.value P0 s) (Subgame.best_response t 0 prof);
      close 1e-9 "br1" (B.value P1 s) (Subgame.best_response t 1 prof))
    [ 0; 3; 20 ]

let clairvoyance () =
  let cfg =
    {
      Bet_tree.pot = 4.;
      stack = 4.;
      streets = [ { bets = [ Pot 1. ]; raises = []; cap = 1 } ];
    }
  in
  let board = Hand.parse "Ks Th 7d 4c 2s" in
  let p0 = Range.of_string "KdKc, 6h5h" and p1 = Range.of_string "KhJc" in
  let t = Subgame.create ~algorithm:Cfr.Plus cfg ~board p0 p1 in
  Subgame.run t 4000;
  let prof = Subgame.average t in
  let s = Subgame.strategy t prof in
  let prob key a = (Option.get (Strategy.find s key)).probs.(a) in
  close 1e-3 "game value" 1. (Subgame.value t prof);
  close 1e-3 "exploitability" 0. (Subgame.exploitability t prof);
  close 1e-3 "value bet" 1. (prob "KdKc:" 1);
  close 0.02 "bluff" 0.5 (prob "6h5h:" 1);
  close 0.02 "call" 0.5 (prob "KhJc:b4" 1)

let converges () =
  let board = Hand.parse "Qs Jh 8d 5c 2s" in
  let t =
    Subgame.create ~algorithm:Cfr.dcfr river ~board (Range.full ())
      (Range.full ())
  in
  let pot = Subgame.pot t in
  let at n =
    Subgame.run t (n - Subgame.iterations t);
    100. *. Subgame.exploitability t (Subgame.average t) /. pot
  in
  let e10 = at 10 and e100 = at 100 and e400 = at 400 in
  if not (e10 > e100 && e100 > e400 && e400 < 0.1) then
    Alcotest.failf "exploitability %% pot: %g, %g, %g" e10 e100 e400

let invalid () =
  let fails name f =
    match f () with
    | _ -> Alcotest.failf "%s accepted" name
    | exception Invalid_argument _ -> ()
  in
  fails "street count" (fun () -> Subgame.create turn ~board:river_board r0 r1);
  fails "short board" (fun () ->
      Subgame.create river ~board:(Hand.parse "Ah Kd") r0 r1);
  fails "repeated card" (fun () ->
      Subgame.create river ~board:(Hand.parse "Ah Ah 7c 7s 2h") r0 r1);
  fails "no pairs" (fun () ->
      Subgame.create river ~board:river_board (Range.of_string "AsAd")
        (Range.of_string "AsAc"))

let tests =
  [
    Alcotest.test_case "river cfr" `Quick
      (matches_scalar_cfr river river_board r0 r1 Cfr.Vanilla 25);
    Alcotest.test_case "river cfr+" `Quick
      (matches_scalar_cfr river river_board r0 r1 Cfr.Plus 25);
    Alcotest.test_case "river dcfr" `Quick
      (matches_scalar_cfr river river_board r0 r1 Cfr.dcfr 25);
    Alcotest.test_case "river values" `Quick
      (matches_scalar_values river river_board r0 r1);
    Alcotest.test_case "turn cfr" `Slow
      (matches_scalar_cfr turn turn_board small0 small1 Cfr.Vanilla 3);
    Alcotest.test_case "turn cfr+" `Slow
      (matches_scalar_cfr turn turn_board small0 small1 Cfr.Plus 3);
    Alcotest.test_case "turn values" `Slow
      (matches_scalar_values turn turn_board small0 small1);
    Alcotest.test_case "clairvoyance" `Quick clairvoyance;
    Alcotest.test_case "converges" `Quick converges;
    Alcotest.test_case "invalid" `Quick invalid;
  ]
