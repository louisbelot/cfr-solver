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

let real0 = Range.of_string "TT+, AQ+, KQs, QJs, JTs, T9s, 98s"
let real1 = Range.of_string "88+, AJ+, KQ, QJs, JTs, A5s"

let prunes_exactly algorithm () =
  let make prune =
    Subgame.create ~algorithm ~prune turn ~board:mono_turn real0 real1
  in
  let a = make true and b = make false in
  Subgame.run a 40;
  Subgame.run b 40;
  if Subgame.skipped a = 0 then Alcotest.fail "nothing pruned";
  Alcotest.(check int) "unpruned" 0 (Subgame.skipped b);
  let pa = Subgame.average a and pb = Subgame.average b in
  if pa <> pb then Alcotest.fail "average strategies differ";
  if Subgame.current a <> Subgame.current b then Alcotest.fail "regrets differ";
  for i = 0 to 1 do
    let x = Subgame.best_response a i pa and y = Subgame.best_response b i pb in
    if x <> y then Alcotest.failf "best response %d: %.17g against %.17g" i x y
  done

let one_bet = Bet_tree.limit ~cap:1 ~pot:4. ~bet:1. 1

let no_limit =
  let sizes = Bet_tree.[ Pot 0.5; Pot 1.; Pot 2.; All_in ] in
  {
    Bet_tree.pot = 4.;
    stack = 12.;
    streets = [ { bets = sizes; raises = sizes; cap = 3 } ];
  }

let ran_windows t =
  if Subgame.pruned t = 0 then Alcotest.fail "nothing pruned";
  if Subgame.caught_up t = 0 then Alcotest.fail "no catch-up"

let rbp_matches_plain algorithm () =
  let make rbp =
    Subgame.create ~algorithm ~rbp one_bet ~board:river_board r1 r0
  in
  let a = make true and b = make false in
  Subgame.run a 400;
  Subgame.run b 400;
  ran_windows a;
  Alcotest.(check int) "plain" 0 (Subgame.pruned b);
  let pa = Subgame.average a and pb = Subgame.average b in
  same_strategy "average" (Subgame.strategy b pb) (Subgame.strategy a pa);
  for i = 0 to 1 do
    close 1e-9 "best response"
      (Subgame.best_response b i pb)
      (Subgame.best_response a i pa)
  done

let rbp_converges algorithm () =
  let make rbp =
    Subgame.create ~algorithm ~rbp no_limit ~board:river_board r0 r1
  in
  let a = make true and b = make false in
  Subgame.run a 2000;
  Subgame.run b 2000;
  ran_windows a;
  let ea = Subgame.exploitability a (Subgame.average a)
  and eb = Subgame.exploitability b (Subgame.average b) in
  if not (ea < 1.33 *. eb && eb < 1.33 *. ea) then
    Alcotest.failf "exploitability %g with pruning, %g without" ea eb

let rbp_folds () =
  let make iso =
    Subgame.create ~algorithm:Cfr.Vanilla ~iso ~rbp:true turn ~board:mono_turn
      sym0 sym1
  in
  let full = make false and folded = make true in
  Subgame.run full 60;
  Subgame.run folded 60;
  ran_windows folded;
  let pf = Subgame.average full and pg = Subgame.average folded in
  same_strategy "folded" (Subgame.strategy full pf) (Subgame.strategy folded pg);
  close 1e-9 "value" (Subgame.value full pf) (Subgame.value folded pg)

let rbp_reads () =
  let make () =
    Subgame.create ~algorithm:Cfr.Linear ~rbp:true no_limit ~board:river_board
      r0 r1
  in
  let a = make () and b = make () in
  for _ = 1 to 200 do
    Subgame.iterate a;
    ignore (Subgame.average a)
  done;
  Subgame.run b 200;
  ran_windows b;
  if Subgame.current a <> Subgame.current b then
    Alcotest.fail "reading the average changed the regrets";
  same_strategy "average"
    (Subgame.strategy b (Subgame.average b))
    (Subgame.strategy a (Subgame.average a))

let bucketed_matches_scalar buckets algorithm iterations () =
  let t =
    Subgame.create ~algorithm ~iso:false ~buckets turn ~board:turn_board small0
      small1
  in
  let label p dealt hand = Subgame.bucket t p dealt hand in
  let (module G) =
    Subgame_ref.make ~label turn ~board:turn_board small0 small1
  in
  let module C = Cfr.Make (G) in
  let c = C.create ~algorithm () in
  C.run c iterations;
  Subgame.run t iterations;
  let players = Hashtbl.create 64 in
  List.iter
    (fun (l : Subgame.line) -> Hashtbl.replace players l.path l.player)
    (Subgame.lines ~chance:true t (Subgame.current t));
  let expected = C.current_strategy c in
  let merged = ref 0 in
  List.iter
    (fun (key, (e : Strategy.entry)) ->
      let i = String.index key ':' in
      let combo = String.sub key 0 i
      and path = String.sub key (i + 1) (String.length key - i - 1) in
      let hand =
        ( Hand.of_string (String.sub combo 0 2),
          Hand.of_string (String.sub combo 2 2) )
      in
      let dealt =
        match String.index_opt path '/' with
        | Some j -> [ Hand.of_string (String.sub path (j + 1) 2) ]
        | None -> []
      in
      let scalar =
        match label (Hashtbl.find players path) dealt hand with
        | Some b ->
            incr merged;
            Printf.sprintf "b%d:%s" b path
        | None -> key
      in
      match Strategy.find expected scalar with
      | None -> Alcotest.failf "no %s for %s" scalar key
      | Some g ->
          Array.iteri
            (fun a p -> close 1e-9 (Printf.sprintf "%s %d" key a) g.probs.(a) p)
            e.probs)
    (Strategy.bindings (Subgame.strategy t (Subgame.current t)));
  if !merged = 0 then Alcotest.fail "no bucketed infoset"

let one_combo_buckets () =
  let t =
    Subgame.create ~algorithm:Cfr.Plus ~buckets:[ 2; 2 ] turn ~board:turn_board
      (Range.of_string "QsQd") small1
  in
  Subgame.run t 5;
  let prof = Subgame.average t in
  Array.iter
    (Array.iter
       (Array.iter (fun p -> if Float.is_nan p then Alcotest.fail "nan")))
    prof;
  if not (Subgame.exploitability t prof >= 0.) then
    Alcotest.fail "exploitability"

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
        (Range.of_string "AsAc"));
  fails "rbp with cfr+" (fun () ->
      Subgame.create ~algorithm:Cfr.Plus ~rbp:true river ~board:river_board r0
        r1);
  fails "rbp with dcfr" (fun () ->
      Subgame.create ~algorithm:Cfr.dcfr ~rbp:true river ~board:river_board r0
        r1);
  fails "rbp with buckets" (fun () ->
      Subgame.create ~rbp:true ~buckets:[ 2; 2 ] turn ~board:turn_board r0 r1)

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
    Alcotest.test_case "pruning cfr+" `Quick (prunes_exactly Cfr.Plus);
    Alcotest.test_case "pruning dcfr" `Quick (prunes_exactly Cfr.dcfr);
    Alcotest.test_case "rbp exact cfr" `Quick (rbp_matches_plain Cfr.Vanilla);
    Alcotest.test_case "rbp exact lcfr" `Quick (rbp_matches_plain Cfr.Linear);
    Alcotest.test_case "rbp converges cfr" `Quick (rbp_converges Cfr.Vanilla);
    Alcotest.test_case "rbp converges lcfr" `Quick (rbp_converges Cfr.Linear);
    Alcotest.test_case "rbp folded" `Quick rbp_folds;
    Alcotest.test_case "rbp reads" `Quick rbp_reads;
    Alcotest.test_case "bucketed turn" `Slow
      (bucketed_matches_scalar [ 3; 0 ] Cfr.Plus 3);
    Alcotest.test_case "bucketed river" `Slow
      (bucketed_matches_scalar [ 0; 4 ] Cfr.Vanilla 3);
    Alcotest.test_case "bucketed both" `Slow
      (bucketed_matches_scalar [ 2; 3 ] Cfr.dcfr 3);
    Alcotest.test_case "one combo in buckets" `Quick one_combo_buckets;
    Alcotest.test_case "clairvoyance" `Quick clairvoyance;
    Alcotest.test_case "converges" `Quick converges;
    Alcotest.test_case "invalid" `Quick invalid;
  ]
