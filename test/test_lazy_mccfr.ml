open Cfr_solver
module K3 = (val Kuhn_n.game 3)
module L3 = (val Leduc_n.game 3)
module B3 = Multi_br.Make (K3)
module E3 = Multi.Eval (K3)

let gap tree_side lazy_side =
  List.fold_left
    (fun acc (key, (e : Strategy.entry)) ->
      let n = Array.length e.probs in
      let q = Strategy.probs lazy_side key ~n in
      let m = ref acc in
      Array.iteri
        (fun a p -> m := Float.max !m (Float.abs (p -. q.(a))))
        e.probs;
      !m)
    0.
    (Strategy.bindings tree_side)

let matches (module G : Multi.S) n () =
  let module T = Multi_mccfr.Make (G) in
  let module Z = Lazy_mccfr.Make (Lazy_mccfr.Of_multi (G)) in
  List.iter
    (fun (weighting, interval) ->
      let t = T.create ~seed:3 ~weighting ~interval () in
      let z = Z.create ~seed:3 ~weighting ~interval () in
      T.run t n;
      Z.run z n;
      Alcotest.(check int) "touched" (T.touched t) (Z.touched z);
      Alcotest.(check bool) "infosets" true (Z.infosets z <= T.infosets t);
      Alcotest.(check (float 0.))
        "average" 0.
        (gap (T.average_strategy t) (Z.average_strategy z));
      Alcotest.(check (float 0.))
        "current" 0.
        (gap (T.current_strategy t) (Z.current_strategy z)))
    [ (Cfr.Vanilla, 1); (Cfr.dcfr, 10); (Linear, 1) ]

let kuhn_two () =
  let module G = Multi.Of_game (Kuhn) in
  let module Z = Lazy_mccfr.Make (Lazy_mccfr.Of_multi (G)) in
  let module B = Multi_br.Make (G) in
  let z = Z.create ~seed:11 () in
  Z.run z 50_000;
  let sigma = Z.average_strategy z in
  Alcotest.(check bool) "nash conv" true (B.nash_conv sigma < 5e-3);
  let module E = Multi.Eval (G) in
  Alcotest.(check (float 5e-3)) "value" (-1. /. 18.) (E.values sigma).(0)

let random_profile seed =
  let rng = Rng.make seed in
  List.fold_left
    (fun s p ->
      List.fold_left
        (fun s (key, actions) ->
          let x = 0.05 +. (0.9 *. Rng.float rng) in
          Strategy.add key ~actions ~probs:[| x; 1. -. x |] s)
        s (E3.infosets p))
    Strategy.empty [ 0; 1; 2 ]

let responds () =
  let module Z = Lazy_mccfr.Make (Lazy_mccfr.Of_multi (K3)) in
  let sigma = random_profile 7 in
  let frozen key n = Strategy.probs sigma key ~n in
  let z =
    Z.create ~seed:2 ~fixed:(fun p -> if p = 1 then None else Some frozen) ()
  in
  Z.run z 20_000;
  let learned =
    List.fold_left
      (fun s (key, (e : Strategy.entry)) ->
        Strategy.add key ~actions:e.actions ~probs:e.probs s)
      sigma
      (Strategy.bindings (Z.average_strategy z))
  in
  let best = B3.value 1 sigma and got = (E3.values learned).(1) in
  Alcotest.(check bool) "near best" true (best -. got < 0.01);
  Alcotest.(check bool)
    "only learner" true
    (List.for_all
       (fun (key, _) -> List.mem_assoc key (E3.infosets 1))
       (Strategy.bindings (Z.average_strategy z)))

let playout () =
  let module Z = Lazy_mccfr.Make (Lazy_mccfr.Of_multi (K3)) in
  let sigma = random_profile 4 in
  let rng = Rng.make 9 and n = 200_000 in
  let sum = Array.make 3 0. in
  for _ = 1 to n do
    Array.iteri
      (fun i u -> sum.(i) <- sum.(i) +. u)
      (Z.playout rng (fun _ key n -> Strategy.probs sigma key ~n))
  done;
  let exact = E3.values sigma in
  Array.iteri
    (fun i s ->
      Alcotest.(check (float 0.01)) "mean" exact.(i) (s /. float_of_int n))
    sum

let tests =
  [
    Alcotest.test_case "matches kuhn3 tree" `Quick (matches (module K3) 3000);
    Alcotest.test_case "matches leduc3 tree" `Quick (matches (module L3) 200);
    Alcotest.test_case "two-player kuhn" `Quick kuhn_two;
    Alcotest.test_case "fixed seats" `Quick responds;
    Alcotest.test_case "playout mean" `Quick playout;
  ]
