open Cfr_solver

let q = Rat.of_ints
let rat = Alcotest.testable Rat.pp Rat.equal
let grid rows = Array.map (Array.map Rat.of_int) rows

let solved name a x y v =
  let x', y', v' = Psro.matrix a in
  Alcotest.check rat (name ^ " value") v v';
  Alcotest.(check (array rat)) (name ^ " row") x x';
  Alcotest.(check (array rat)) (name ^ " column") y y'

let matrices () =
  let third = q 1 3 in
  solved "rps"
    (grid [| [| 0; -1; 1 |]; [| 1; 0; -1 |]; [| -1; 1; 0 |] |])
    [| third; third; third |] [| third; third; third |] Rat.zero;
  solved "2x2"
    (grid [| [| 3; -1 |]; [| -2; 1 |] |])
    [| q 3 7; q 4 7 |]
    [| q 2 7; q 5 7 |]
    (q 1 7);
  solved "saddle"
    (grid [| [| 4; 2; 5 |]; [| 1; 0; 7 |] |])
    [| Rat.one; Rat.zero |]
    [| Rat.zero; Rat.one; Rat.zero |]
    (Rat.of_int 2)

let random_strategy rng (tree : Tree.t) player =
  Array.fold_left
    (fun s (i : Tree.infoset) ->
      if i.player <> player then s
      else
        let w = Array.map (fun _ -> Rng.float rng) i.actions in
        let t = Array.fold_left ( +. ) 0. w in
        Strategy.add i.key ~actions:i.actions
          ~probs:(Array.map (fun x -> x /. t) w)
          s)
    Strategy.empty tree.infosets

let mixture_is_linear () =
  let g = (module Leduc : Game.S) in
  let tree = Tree.build g and ev = Restricted.evaluator g ~exploiter:P0 in
  let rng = Rng.make 5 in
  for _ = 1 to 5 do
    let parts =
      List.init 4 (fun k ->
          ( Rng.float rng,
            if k = 0 then Psro.pure tree P0 else random_strategy rng tree P0 ))
    in
    let total = List.fold_left (fun t (w, _) -> t +. w) 0. parts in
    let opp = random_strategy rng tree P1 in
    let expected =
      List.fold_left
        (fun acc (w, s) -> acc +. (w /. total *. ev.against s opp))
        0. parts
    in
    Alcotest.(check (float 1e-12))
      "value of the mix" expected
      (ev.against (Psro.mixture tree P0 parts) opp)
  done

let last steps = List.nth steps (List.length steps - 1)

let double_oracle () =
  let steps, profile = Psro.run ~meta:Exact (module Kuhn) ~iterations:50 in
  let s : Psro.step = last steps in
  Alcotest.(check (float 1e-12)) "value" (-1. /. 18.) s.value;
  Alcotest.(check (float 1e-12)) "nash conv" 0. s.nash_conv;
  Alcotest.(check bool) "stops early" true (s.iteration < 20);
  let module B = Best_response.Make (Kuhn) in
  Alcotest.(check (float 1e-12)) "profile" 0. (B.nash_conv profile)

let float_meta () =
  let steps, _ = Psro.run ~meta:Nash (module Kuhn) ~iterations:50 in
  let s : Psro.step = last steps in
  Alcotest.(check bool) "stops early" true (s.iteration < 20);
  Alcotest.(check (float 1e-12)) "value" (-1. /. 18.) s.value;
  Alcotest.(check bool) "nash conv" true (s.nash_conv < 1e-12)

let uniform_meta () =
  let steps, _ = Psro.run ~meta:Uniform (module Kuhn) ~iterations:30 in
  Alcotest.(check int) "never stops" 31 (List.length steps);
  let s : Psro.step = last steps in
  Alcotest.(check (pair int int)) "duplicates kept" (31, 31) s.sizes;
  Alcotest.(check bool) "slower than double oracle" true (s.nash_conv > 1e-3)

let tests =
  [
    Alcotest.test_case "matrix games" `Quick matrices;
    Alcotest.test_case "mixture is linear" `Quick mixture_is_linear;
    Alcotest.test_case "double oracle on kuhn" `Quick double_oracle;
    Alcotest.test_case "float meta solver" `Quick float_meta;
    Alcotest.test_case "uniform meta solver" `Quick uniform_meta;
  ]
