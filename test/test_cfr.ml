open Cfr_solver
module C = Cfr.Make (Kuhn)
module E = Eval.Make (Kuhn)

let solve n =
  let t = C.create () in
  C.run t n;
  t

let bet s key = (Strategy.probs s key ~n:2).(1)

let close tol name expected actual =
  if Float.abs (expected -. actual) > tol then
    Alcotest.failf "%s: expected %.4f, got %.4f (tolerance %g)" name expected
      actual tol

let first_iteration_is_uniform () =
  let t = solve 1 in
  Alcotest.(check int)
    "every infoset" 12
    (Strategy.cardinal (C.average_strategy t));
  List.iter
    (fun (key, { Strategy.probs; _ }) ->
      Alcotest.(check (array (float 0.))) key [| 0.5; 0.5 |] probs)
    (Strategy.bindings (C.average_strategy t))

let openspiel_two_iterations () =
  let s = C.average_strategy (solve 2) in
  Alcotest.(check (array (float 1e-12)))
    "Qb" [| 0.25; 0.75 |]
    (Strategy.probs s "Qb" ~n:2)

let game_value () =
  let s = C.average_strategy (solve 10_000) in
  close 1e-4 "value" (-1. /. 18.) (E.expected_value s)

let lands_in_equilibrium_family () =
  let s = C.average_strategy (solve 10_000) in
  let alpha = bet s "J" in
  if alpha < 0. || alpha > 1. /. 3. then Alcotest.failf "alpha = %g" alpha;
  close 0.03 "K bets 3 alpha" (3. *. alpha) (bet s "K");
  close 0.02 "Q calls alpha + 1/3" (alpha +. (1. /. 3.)) (bet s "Qpb");
  close 0.01 "Q checks" 0. (bet s "Q");
  close 0.01 "J folds" 0. (bet s "Jpb");
  close 0.01 "K calls" 1. (bet s "Kpb");
  close 0.01 "player 1 bluffs J" (1. /. 3.) (bet s "Jp");
  close 0.01 "player 1 checks Q" 0. (bet s "Qp");
  close 0.01 "player 1 bets K" 1. (bet s "Kp");
  close 0.01 "player 1 folds J" 0. (bet s "Jb");
  close 0.01 "player 1 calls Q" (1. /. 3.) (bet s "Qb");
  close 0.01 "player 1 calls K" 1. (bet s "Kb")

module R = Reference_cfr.Make (Kuhn)
module B = Best_response.Make (Kuhn)

let algorithms =
  Cfr.[ Vanilla; Plus; Linear; dcfr; dcfr_plus; pcfr_plus; pdcfr_plus ]

let solve_with algorithm n =
  let t = C.create ~algorithm () in
  C.run t n;
  C.average_strategy t

let matches_reference algorithm () =
  let s = solve_with algorithm 30 in
  List.iter
    (fun (key, expected) ->
      Alcotest.(check (array (float 1e-10)))
        key expected
        (Strategy.probs s key ~n:(Array.length expected)))
    (R.run algorithm 30)

let random_dcfr =
  QCheck2.Gen.(
    map3
      (fun alpha beta gamma -> Cfr.Discounted { alpha; beta; gamma })
      (float_range 0. 3.) (float_range (-1.) 3.) (float_range 0. 4.))

let dcfr_matches_reference () =
  QCheck2.Test.check_exn
  @@ QCheck2.Test.make ~count:20 ~name:"dcfr" random_dcfr (fun algorithm ->
      let s = solve_with algorithm 8 in
      List.for_all
        (fun (key, expected) ->
          let got = Strategy.probs s key ~n:(Array.length expected) in
          Array.for_all2 (fun x y -> Float.abs (x -. y) < 1e-10) expected got)
        (R.run algorithm 8))

let random_predictive =
  QCheck2.Gen.(
    map2
      (fun alpha gamma -> Cfr.Predictive { alpha; gamma })
      (oneof [ float_range 0. 4.; pure infinity ])
      (float_range 0. 6.))

let predictive_matches_reference () =
  QCheck2.Test.check_exn
  @@ QCheck2.Test.make ~count:20 ~name:"predictive" random_predictive
       (fun algorithm ->
         let s = solve_with algorithm 12 in
         List.for_all
           (fun (key, expected) ->
             let got = Strategy.probs s key ~n:(Array.length expected) in
             Array.for_all2 (fun x y -> Float.abs (x -. y) < 1e-10) expected got)
           (R.run algorithm 12))

let every_algorithm_starts_uniform () =
  List.iter
    (fun algorithm ->
      List.iter
        (fun (key, { Strategy.probs; _ }) ->
          Alcotest.(check (array (float 0.))) key [| 0.5; 0.5 |] probs)
        (Strategy.bindings (solve_with algorithm 1)))
    algorithms

let openspiel_cfr_plus () =
  let s = solve_with Plus 200 in
  close 1e-3 "value" (-1. /. 18.) (E.expected_value s);
  let e = B.exploitability s in
  if e > 1e-3 then Alcotest.failf "exploitability %g" e

let variants_beat_vanilla () =
  let e alg = B.exploitability (solve_with alg 1000) in
  let base = e Vanilla in
  List.iter
    (fun alg ->
      let x = e alg in
      if x > base /. 5. then
        Alcotest.failf "%s: %g vs vanilla %g" (Cfr.algorithm_name alg) x base)
    Cfr.[ Plus; Linear; dcfr ]

let predictive_on_kuhn () =
  let e alg = B.exploitability (solve_with alg 1000) in
  let plus = e Plus and pcfr = e Cfr.pcfr_plus and pdcfr = e Cfr.pdcfr_plus in
  if pcfr > 1e-6 || pcfr > plus /. 100. then
    Alcotest.failf "pcfr+ %g, cfr+ %g" pcfr plus;
  if pdcfr > 1e-9 then Alcotest.failf "pdcfr+ %g" pdcfr

let predictive_last_iterate () =
  let last alg =
    let t = C.create ~algorithm:alg () in
    C.run t 2000;
    B.exploitability (C.current_strategy t)
  in
  let plus = last Plus and pcfr = last Cfr.pcfr_plus in
  if pcfr > 1e-12 || plus < 1e-3 then
    Alcotest.failf "pcfr+ last %g, cfr+ last %g" pcfr plus

module CL = Cfr.Make (Leduc)
module BL = Best_response.Make (Leduc)

let pdcfr_overtakes_on_leduc () =
  let e alg n =
    let t = CL.create ~algorithm:alg () in
    CL.run t n;
    BL.exploitability (CL.average_strategy t)
  in
  let early = e Cfr.pdcfr_plus 1000 and dcfr_early = e Cfr.dcfr 1000 in
  if early < dcfr_early then
    Alcotest.failf "pdcfr+ %g already ahead of dcfr %g" early dcfr_early;
  let late = e Cfr.pdcfr_plus 30_000 and dcfr_late = e Cfr.dcfr 30_000 in
  if late > dcfr_late /. 10. then
    Alcotest.failf "pdcfr+ %g, dcfr %g" late dcfr_late

let samplers_reject_prediction () =
  let module M = Mccfr.Make (Kuhn) in
  let module P = Parallel.Make (Kuhn) in
  let rejects name f =
    match f () with
    | () -> Alcotest.failf "%s accepted a predictive weighting" name
    | exception Invalid_argument _ -> ()
  in
  rejects "mccfr" (fun () ->
      ignore (M.create ~weighting:Cfr.pcfr_plus Mccfr.External));
  rejects "parallel" (fun () ->
      ignore (P.create ~weighting:Cfr.pdcfr_plus ~domains:1 Mccfr.External))

let current_is_predicted () =
  let t = C.create ~algorithm:Cfr.pcfr_plus () in
  C.run t 7;
  List.iter
    (fun (key, expected) ->
      Alcotest.(check (array (float 1e-12)))
        key expected
        (Strategy.probs (C.current_strategy t) key ~n:(Array.length expected)))
    (R.current Cfr.pcfr_plus 7)

let touched () =
  let after algorithm =
    let t = C.create ~algorithm () in
    C.run t 3;
    C.touched t
  in
  Alcotest.(check int) "vanilla" (3 * 55) (after Vanilla);
  Alcotest.(check int) "cfr+" (6 * 55) (after Plus)

let tests =
  List.map
    (fun alg ->
      Alcotest.test_case
        (Cfr.algorithm_name alg ^ " matches reference")
        `Quick (matches_reference alg))
    algorithms
  @ [
      Alcotest.test_case "random dcfr matches reference" `Quick
        dcfr_matches_reference;
      Alcotest.test_case "random predictive matches reference" `Quick
        predictive_matches_reference;
      Alcotest.test_case "first iteration is uniform" `Quick
        first_iteration_is_uniform;
      Alcotest.test_case "OpenSpiel two iterations" `Quick
        openspiel_two_iterations;
      Alcotest.test_case "game value -1/18" `Quick game_value;
      Alcotest.test_case "equilibrium family" `Quick lands_in_equilibrium_family;
      Alcotest.test_case "variants start uniform" `Quick
        every_algorithm_starts_uniform;
      Alcotest.test_case "OpenSpiel CFR+" `Quick openspiel_cfr_plus;
      Alcotest.test_case "predictive on kuhn" `Quick predictive_on_kuhn;
      Alcotest.test_case "predictive last iterate" `Quick
        predictive_last_iterate;
      Alcotest.test_case "pdcfr+ overtakes dcfr on leduc" `Slow
        pdcfr_overtakes_on_leduc;
      Alcotest.test_case "samplers reject prediction" `Quick
        samplers_reject_prediction;
      Alcotest.test_case "current strategy is predicted" `Quick
        current_is_predicted;
      Alcotest.test_case "variants beat vanilla" `Quick variants_beat_vanilla;
      Alcotest.test_case "nodes touched" `Quick touched;
    ]
