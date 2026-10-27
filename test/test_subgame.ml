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

let mono_turn = Hand.parse "Ks 9s 5s 2s"
let mono_flop = Hand.parse "Ks 9s 5s"
let flop = Bet_tree.limit ~cap:1 ~pot:4. ~bet:1. 3
let sym0 = Range.of_string "AA:0.9, KQs, QJs:0.6, T9s, 76o:0.3, 33"
let sym1 = Range.of_string "QQ:0.8, AKs, JTs, 98s:0.7, A5o:0.4"

let folds_exactly cfg board r0 r1 algorithm iterations () =
  let full = Subgame.create ~algorithm ~iso:false cfg ~board r0 r1 in
  let folded = Subgame.create ~algorithm cfg ~board r0 r1 in
  if Subgame.decisions folded >= Subgame.decisions full then
    Alcotest.failf "%d decisions folded, %d full" (Subgame.decisions folded)
      (Subgame.decisions full);
  Subgame.run full iterations;
  Subgame.run folded iterations;
  let pf = Subgame.average full and pg = Subgame.average folded in
  let sf = Subgame.strategy full pf and sg = Subgame.strategy folded pg in
  Alcotest.(check int)
    "infosets"
    (List.length (Strategy.bindings sf))
    (List.length (Strategy.bindings sg));
  same_strategy (Cfr.algorithm_name algorithm) sf sg;
  close 1e-9 "value" (Subgame.value full pf) (Subgame.value folded pg);
  for i = 0 to 1 do
    close 1e-9 "best response"
      (Subgame.best_response full i pf)
      (Subgame.best_response folded i pg)
  done

let symmetric_strategy () =
  let t = Subgame.create ~algorithm:Cfr.dcfr turn ~board:mono_turn sym0 sym1 in
  Subgame.run t 20;
  let s = Subgame.strategy t (Subgame.average t) in
  let group =
    Subgame.symmetries (Array.to_list mono_turn) (Subgame.range t 0)
      (Subgame.range t 1)
  in
  Alcotest.(check int) "group" 6 (List.length group);
  let checked = ref 0 in
  List.iter
    (fun (key, (e : Strategy.entry)) ->
      match String.split_on_char ':' key with
      | [ combo; path ] when not (String.contains path '/') ->
          let a = Hand.of_string (String.sub combo 0 2)
          and b = Hand.of_string (String.sub combo 2 2) in
          List.iter
            (fun p ->
              let image = Subgame.key (Suit.combo p (a, b)) path in
              match Strategy.find s image with
              | Some g when g.probs = e.probs -> incr checked
              | _ -> Alcotest.failf "%s and %s differ" key image)
            group
      | _ -> ())
    (Strategy.bindings s);
  if !checked = 0 then Alcotest.fail "nothing checked"

let asymmetric_ranges () =
  let r = Range.of_string "AhAd:0.3, AhAc:0.5, AdAc" in
  let a = Subgame.create turn ~board:mono_turn r sym1 in
  let b = Subgame.create ~iso:false turn ~board:mono_turn r sym1 in
  Alcotest.(check int) "decisions" (Subgame.decisions b) (Subgame.decisions a)

let single_precision () =
  let make single =
    Subgame.create ~algorithm:Cfr.dcfr ~single turn ~board:mono_turn sym0 sym1
  in
  let d = make false and f = make true in
  Alcotest.(check int) "half the bytes" (Subgame.memory d) (2 * Subgame.memory f);
  let worst pd pf =
    let w = ref 0. in
    Array.iteri
      (fun i node ->
        Array.iteri
          (fun a row ->
            Array.iteri
              (fun h x -> w := Float.max !w (Float.abs (x -. pf.(i).(a).(h))))
              row)
          node)
      pd;
    !w
  in
  Subgame.run d 2;
  Subgame.run f 2;
  let e = worst (Subgame.average d) (Subgame.average f) in
  if not (e <= 1e-6) then Alcotest.failf "after 2 iterations: %g" e;
  Subgame.run d 28;
  Subgame.run f 28;
  let ed = Subgame.exploitability d (Subgame.average d)
  and ef = Subgame.exploitability f (Subgame.average f) in
  if not (Float.abs (ed -. ef) <= 0.01 *. ed) then
    Alcotest.failf "exploitability %g against %g" ef ed

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
    Alcotest.test_case "folded turn cfr" `Quick
      (folds_exactly turn mono_turn sym0 sym1 Cfr.Vanilla 6);
    Alcotest.test_case "folded turn cfr+" `Quick
      (folds_exactly turn mono_turn sym0 sym1 Cfr.Plus 6);
    Alcotest.test_case "folded turn dcfr" `Quick
      (folds_exactly turn mono_turn sym0 sym1 Cfr.dcfr 6);
    Alcotest.test_case "folded flop" `Slow
      (folds_exactly flop mono_flop sym0 sym1 Cfr.Plus 2);
    Alcotest.test_case "symmetric strategy" `Quick symmetric_strategy;
    Alcotest.test_case "asymmetric ranges" `Quick asymmetric_ranges;
    Alcotest.test_case "single precision" `Quick single_precision;
    Alcotest.test_case "clairvoyance" `Quick clairvoyance;
    Alcotest.test_case "converges" `Quick converges;
    Alcotest.test_case "invalid" `Quick invalid;
  ]
