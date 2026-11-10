open Cfr_solver
module B = Best_response.Make (Kuhn)

let kuhn = Tree.build (module Kuhn)

let config learner =
  {
    Deep_cfr.traversals = 50;
    capacity = 100_000;
    learner;
    average_steps = 300;
    domains = 1;
    seed = 4;
  }

let small =
  Deep_cfr.Network { hidden = [ 32 ]; steps = 300; batch = 64; lr = 3e-3 }

let regret_match () =
  let probs = Alcotest.(array (float 1e-12)) in
  Alcotest.check probs "positive part" [| 0.75; 0.; 0.25 |]
    (Deep_cfr.regret_match [| 3.; -2.; 1. |]);
  Alcotest.check probs "argmax" [| 0.; 1.; 0. |]
    (Deep_cfr.regret_match [| -3.; -0.5; -1. |])

let reference_average snapshots =
  let sums = Hashtbl.create 16 in
  let rec go p reach t sigma h =
    match Kuhn.node h with
    | Game.Terminal _ -> ()
    | Chance outcomes ->
        List.iter (fun (_, h') -> go p reach t sigma h') outcomes
    | Decision { player; infoset; actions } ->
        let probs = Strategy.probs sigma infoset ~n:(List.length actions) in
        if player = p then begin
          let acc =
            match Hashtbl.find_opt sums infoset with
            | Some a -> a
            | None ->
                let a = Array.make (List.length actions) 0. in
                Hashtbl.add sums infoset a;
                a
          in
          Array.iteri
            (fun a x -> acc.(a) <- acc.(a) +. (float_of_int t *. reach *. x))
            probs
        end;
        List.iteri
          (fun a (_, h') ->
            go p (if player = p then reach *. probs.(a) else reach) t sigma h')
          actions
  in
  List.iteri
    (fun i sigma ->
      List.iter (fun p -> go p 1. (i + 1) sigma Kuhn.root) [ Game.P0; P1 ])
    snapshots;
  sums

let sd_average_matches_reference () =
  let d = Deep_cfr.create ~config:(config Deep_cfr.Exact) Features.kuhn kuhn in
  let snapshots =
    List.init 12 (fun _ ->
        let s = Deep_cfr.current d in
        Deep_cfr.iterate d;
        s)
  in
  let avg = Deep_cfr.average_exact d in
  Hashtbl.iter
    (fun key sum ->
      let total = Array.fold_left ( +. ) 0. sum in
      Alcotest.(check (array (float 1e-12)))
        key
        (Array.map (fun x -> x /. total) sum)
        (Strategy.probs avg key ~n:(Array.length sum)))
    (reference_average snapshots)

let exact_learner_converges () =
  let d = Deep_cfr.create ~config:(config Deep_cfr.Exact) Features.kuhn kuhn in
  Deep_cfr.run d 200;
  let e = B.exploitability (Deep_cfr.average_exact d) in
  if e > 0.01 then Alcotest.failf "exploitability %g" e

let network_learns_kuhn () =
  let d = Deep_cfr.create ~config:(config small) Features.kuhn kuhn in
  Deep_cfr.run d 80;
  let sd = B.exploitability (Deep_cfr.average_exact d)
  and net = B.exploitability (Deep_cfr.average_network d) in
  if sd > 0.04 || net > 0.04 then Alcotest.failf "sd %g, strategy net %g" sd net

let deterministic () =
  let run peek =
    let d = Deep_cfr.create ~config:(config small) Features.kuhn kuhn in
    Deep_cfr.run d 2;
    if peek then ignore (Deep_cfr.average_network d);
    Deep_cfr.run d 1;
    Strategy.bindings (Deep_cfr.current d)
  in
  Alcotest.(check bool) "same seed, same policy" true (run false = run false);
  Alcotest.(check bool)
    "averaging leaves the run alone" true
    (run false = run true)

let tests =
  [
    Alcotest.test_case "regret matching" `Quick regret_match;
    Alcotest.test_case "sd-cfr average" `Quick sd_average_matches_reference;
    Alcotest.test_case "exact learner converges" `Quick exact_learner_converges;
    Alcotest.test_case "network learns kuhn" `Quick network_learns_kuhn;
    Alcotest.test_case "deterministic" `Quick deterministic;
  ]
