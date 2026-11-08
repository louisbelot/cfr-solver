open Cfr_solver
module K3 = (val Kuhn_n.game 3)
module M3 = Multi_mccfr.Make (K3)
module B3 = Multi_br.Make (K3)
module E3 = Multi.Eval (K3)

let gap a b =
  List.fold_left2
    (fun acc (k, x) (k', y) ->
      if k <> k' then Alcotest.failf "%s vs %s" k k';
      let m = ref acc in
      Array.iteri
        (fun i p -> m := Float.max !m (Float.abs (p -. y.Strategy.probs.(i))))
        x.Strategy.probs;
      !m)
    0. (Strategy.bindings a) (Strategy.bindings b)

let matches_two_player (module G : Game.S) n () =
  let module S = Mccfr.Make (G) in
  let module N = Multi_mccfr.Make (Multi.Of_game (G)) in
  List.iter
    (fun (weighting, interval) ->
      List.iter
        (fun averaging ->
          let s = S.create ~seed:5 ~weighting ~interval External in
          let m = N.create ~seed:5 ~weighting ~interval ~averaging () in
          S.run s n;
          N.run m n;
          Alcotest.(check int) "touched" (S.touched s) (N.touched m);
          Alcotest.(check (float 0.))
            "average" 0.
            (gap (S.average_strategy s) (N.average_strategy m));
          Alcotest.(check (float 0.))
            "current" 0.
            (gap (S.current_strategy s) (N.current_strategy m)))
        [ Multi_mccfr.Simple; Weighted ])
    [ (Cfr.Vanilla, 1); (Cfr.dcfr, 10); (Linear, 1) ]

let profile seed =
  let rng = Rng.make seed in
  List.fold_left
    (fun s p ->
      List.fold_left
        (fun s (key, actions) ->
          let x = 0.05 +. (0.9 *. Rng.float rng) in
          Strategy.add key ~actions ~probs:[| x; 1. -. x |] s)
        s (E3.infosets p))
    Strategy.empty [ 0; 1; 2 ]

let exact_average profiles =
  let sums = Hashtbl.create 64 in
  let add key a w =
    let row =
      match Hashtbl.find_opt sums key with
      | Some row -> row
      | None ->
          let row = Array.make 2 0. in
          Hashtbl.add sums key row;
          row
    in
    row.(a) <- row.(a) +. w
  in
  let rec go sigma h reach =
    match K3.node h with
    | Multi.Terminal _ -> ()
    | Chance outcomes -> List.iter (fun (_, h') -> go sigma h' reach) outcomes
    | Decision { player; infoset; actions } ->
        let probs = Strategy.probs sigma infoset ~n:2 in
        List.iteri
          (fun a (_, h') ->
            add infoset a (reach.(player) *. probs.(a));
            let reach = Array.copy reach in
            reach.(player) <- reach.(player) *. probs.(a);
            go sigma h' reach)
          actions
  in
  List.iter (fun sigma -> go sigma K3.root [| 1.; 1.; 1. |]) profiles;
  Hashtbl.fold
    (fun key row s ->
      Strategy.add key ~actions:[| "p"; "b" |]
        ~probs:(Regret_matching.normalize row)
        s)
    sums Strategy.empty

let averaged averaging rounds a b =
  let t = M3.create ~seed:7 ~averaging () in
  for r = 1 to rounds do
    let sigma = if r mod 2 = 0 then a else b in
    for i = 0 to 2 do
      M3.accumulate t i sigma
    done
  done;
  M3.average_strategy t

let averaging () =
  let a = profile 3 and b = profile 4 in
  let exact = exact_average [ a; b ] in
  let full = averaged Full 2 a b in
  let weighted = averaged Weighted 40_000 a b in
  let simple = averaged Simple 40_000 a b in
  if gap full exact > 1e-12 then Alcotest.failf "full: %g" (gap full exact);
  if gap weighted exact > 0.025 then
    Alcotest.failf "weighted: %g" (gap weighted exact);
  if gap simple exact < 0.03 then Alcotest.failf "simple: %g" (gap simple exact)

let only_traverser () =
  let t = M3.create ~seed:2 () in
  M3.run t 2;
  let before = M3.current_strategy t in
  M3.traverse t 1;
  let after = M3.current_strategy t in
  List.iter
    (fun p ->
      List.iter
        (fun (key, _) ->
          let x = Strategy.probs before key ~n:2
          and y = Strategy.probs after key ~n:2 in
          if p <> 1 && x <> y then Alcotest.failf "%s moved" key)
        (E3.infosets p))
    [ 0; 1; 2 ];
  if gap before after = 0. then Alcotest.fail "player 1 never moved"

let seeded () =
  let run seed =
    let t = M3.create ~seed () in
    M3.run t 200;
    M3.average_strategy t
  in
  Alcotest.(check (float 0.)) "same seed" 0. (gap (run 3) (run 3));
  if gap (run 3) (run 4) = 0. then Alcotest.fail "seeds agree"

let three_player_kuhn () =
  List.iter
    (fun averaging ->
      let t = M3.create ~seed:11 ~averaging () in
      M3.run t 20_000;
      let s = M3.average_strategy t in
      let nc = B3.nash_conv s and u = E3.values s in
      if nc > 0.05 then
        Alcotest.failf "%s: nash conv %g"
          (Multi_mccfr.averaging_name averaging)
          nc;
      if Float.abs (u.(1) +. (1. /. 48.)) > 0.01 then
        Alcotest.failf "second player gets %g" u.(1))
    [ Multi_mccfr.Simple; Weighted; Full ]

let bad_interval () =
  Alcotest.check_raises "interval"
    (Invalid_argument "Multi_mccfr.create: interval must be positive")
    (fun () -> ignore (M3.create ~interval:0 ()))

let tests =
  [
    Alcotest.test_case "kuhn matches two-player mccfr" `Quick
      (matches_two_player (module Kuhn) 2_000);
    Alcotest.test_case "leduc matches two-player mccfr" `Quick
      (matches_two_player (module Leduc) 300);
    Alcotest.test_case "weighted averaging is unbiased" `Quick averaging;
    Alcotest.test_case "traverse moves one player" `Quick only_traverser;
    Alcotest.test_case "seeded" `Quick seeded;
    Alcotest.test_case "three-player kuhn" `Quick three_player_kuhn;
    Alcotest.test_case "bad interval" `Quick bad_interval;
  ]
