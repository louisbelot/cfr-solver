open Cfr_solver
module K = Mccfr.Make (Kuhn)
module P = Parallel.Make (Kuhn)
module B = Best_response.Make (Kuhn)

let same ?(tol = 0.) name a b =
  List.iter2
    (fun (k, x) (_, y) ->
      Alcotest.(check (array (float tol))) (name ^ " " ^ k) x y)
    a b

let one_domain_is_serial () =
  List.iter
    (fun (mode, weighting, sampling) ->
      let p = P.create ~seed:5 ~weighting ~batch:50 ~mode ~domains:1 sampling
      and s = K.create ~seed:5 ~weighting ~interval:50 sampling in
      P.run p 500;
      K.run s 500;
      same "regret" (K.regrets s) (P.regrets p);
      same ~tol:1e-9 "strategy sum" (K.strategy_sums s) (P.strategy_sums p);
      Alcotest.(check int) "touched" (K.touched s) (P.touched p))
    Parallel.
      [
        (Shared, Cfr.Vanilla, Mccfr.External);
        (Private, Vanilla, External);
        (Shared, Linear, Mccfr.outcome);
        (Private, Cfr.dcfr, Mccfr.outcome);
      ]

let rounds_match_manual_merge () =
  let p = P.create ~seed:3 ~batch:100 ~mode:Private ~domains:3 External in
  P.run p 900;
  let first = K.create ~seed:3 External in
  let second = K.split first in
  let third = K.split first in
  let workers = [ first; second; third ] in
  let master = Table.copy (K.table first) in
  for _ = 1 to 3 do
    List.iter (fun w -> K.run w 100) workers;
    let tables = List.map K.table workers in
    let step m get =
      Array.iteri
        (fun k x ->
          m.(k) <-
            x
            +. List.fold_left (fun acc w -> acc +. ((get w).(k) -. x)) 0. tables)
        m
    in
    step master.regret (fun (w : Table.t) -> w.regret);
    step master.strategy_sum (fun (w : Table.t) -> w.strategy_sum);
    List.iter (fun w -> Table.blit ~src:master ~dst:w) tables
  done;
  let close name a b =
    List.iter2
      (fun (k, x) (_, y) ->
        Alcotest.(check (array (float 1e-12))) (name ^ " " ^ k) x y)
      a b
  in
  close "regret" (Table.regrets master) (P.regrets p);
  close "strategy sum" (Table.strategy_sums master) (P.strategy_sums p)

let private_is_deterministic () =
  let probs () =
    let p = P.create ~seed:11 ~batch:100 ~mode:Private ~domains:4 External in
    P.run p 20_000;
    P.run p 7;
    Strategy.bindings (P.average_strategy p)
    |> List.map (fun (key, (e : Strategy.entry)) -> (key, e.probs))
  in
  Alcotest.(check bool) "same strategy" true (probs () = probs ())

let counts () =
  List.iter
    (fun (mode, weighting, rounds) ->
      let p = P.create ~batch:7 ~weighting ~mode ~domains:4 External in
      P.run p 1001;
      P.run p 5;
      Alcotest.(check int) "iterations" 1006 (P.iterations p);
      Alcotest.(check int) "rounds" rounds (P.rounds p))
    Parallel.
      [ (Shared, Cfr.Vanilla, 2); (Shared, Linear, 37); (Private, Vanilla, 37) ]

let converges mode bound () =
  let p = P.create ~mode ~batch:100 ~domains:4 External in
  P.run p 40_000;
  let e = B.exploitability (P.average_strategy p) in
  if not (e <= bound) then
    Alcotest.failf "%s: exploitability %g" (Parallel.mode_name mode) e

module PL = Parallel.Make (Leduc)
module BL = Best_response.Make (Leduc)

let shared_on_leduc () =
  let p = PL.create ~domains:4 External in
  PL.run p 200_000;
  let e = BL.exploitability (PL.average_strategy p) in
  if not (e <= 0.05) then Alcotest.failf "exploitability %g" e

let rejects () =
  Alcotest.check_raises "domains"
    (Invalid_argument "Parallel.create: domains must be positive") (fun () ->
      ignore (P.create ~domains:0 External));
  Alcotest.check_raises "batch"
    (Invalid_argument "Parallel.create: batch must be positive") (fun () ->
      ignore (P.create ~batch:0 ~domains:2 External))

let tests =
  [
    Alcotest.test_case "one domain is serial" `Quick one_domain_is_serial;
    Alcotest.test_case "private rounds merge deltas" `Quick
      rounds_match_manual_merge;
    Alcotest.test_case "private is deterministic" `Quick
      private_is_deterministic;
    Alcotest.test_case "iterations and rounds" `Quick counts;
    Alcotest.test_case "shared converges on kuhn" `Quick
      (converges Shared 0.015);
    Alcotest.test_case "private converges on kuhn" `Quick
      (converges Private 0.03);
    Alcotest.test_case "shared on leduc" `Quick shared_on_leduc;
    Alcotest.test_case "arguments" `Quick rejects;
  ]
