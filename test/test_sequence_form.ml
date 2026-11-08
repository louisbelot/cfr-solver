open Cfr_solver
module S = Sequence_form

let rat = Alcotest.testable Rat.pp Rat.equal
let kuhn = lazy (S.build (module Kuhn))

let leduc2_game =
  (module (val Leduc.game { Leduc.ranks = 2; suits = 2 }) : Game.S)

let leduc2 = lazy (S.build leduc2_game)

let sizes () =
  let t = Lazy.force kuhn in
  Alcotest.(check (list int))
    "kuhn" [ 13; 13; 6; 6 ]
    [ S.sequences t P0; S.sequences t P1; S.infosets t P0; S.infosets t P1 ]

let kuhn_lp () =
  let t = Lazy.force kuhn in
  let sol = S.solve t in
  Alcotest.check rat "value" (Rat.of_ints (-1) 18) sol.value;
  let x, y = sol.plans in
  Alcotest.check rat "nash conv" Rat.zero
    (Rat.add (S.best_response t P0 y) (S.best_response t P1 x));
  let alpha = (Strategy.probs sol.strategy "J" ~n:2).(1) in
  Alcotest.(check bool) "alpha in range" true (alpha >= 0. && alpha <= 1. /. 3.);
  let family = Kuhn.equilibrium alpha in
  List.iter
    (fun (p, plan) ->
      Alcotest.(check (array (float 1e-12)))
        (Game.player_to_string p)
        (Array.map Rat.to_float (S.plan t p family))
        (Array.map Rat.to_float plan))
    [ (Game.P0, x); (P1, y) ]

let family () =
  let t = Lazy.force kuhn in
  List.iter
    (fun alpha ->
      let s = Kuhn.equilibrium alpha in
      let v = S.value t (S.plan t P0 s) (S.plan t P1 s) in
      Alcotest.(check (float 1e-15)) "value" (-1. /. 18.) (Rat.to_float v);
      Alcotest.(check bool)
        "nash conv" true
        (Rat.to_float (S.nash_conv t s) < 1e-15))
    [ 0.; 0.1; 0.25; 1. /. 3. ]

let random_strategy (tree : Tree.t) seed =
  let st = Random.State.make [| seed |] in
  Array.fold_left
    (fun acc (info : Tree.infoset) ->
      let w = Array.map (fun _ -> Random.State.float st 1.) info.actions in
      let total = Array.fold_left ( +. ) 0. w in
      Strategy.add info.key ~actions:info.actions
        ~probs:(Array.map (fun x -> x /. total) w)
        acc)
    Strategy.empty tree.infosets

let against_float (module G : Game.S) count () =
  let module B = Best_response.Make (G) in
  let t = S.build (module G) and tree = Tree.build (module G) in
  for seed = 1 to count do
    let s = random_strategy tree seed in
    Alcotest.(check (float 1e-12))
      "nash conv" (B.nash_conv s)
      (Rat.to_float (S.nash_conv t s));
    Alcotest.(check (float 1e-12))
      "p1 response" (B.value P1 s)
      (Rat.to_float (S.best_response t P1 (S.plan t P0 s)))
  done

let normalizes () =
  let t = Lazy.force kuhn in
  let half = Kuhn.equilibrium 0. in
  let doubled =
    List.fold_left
      (fun acc (key, (e : Strategy.entry)) ->
        Strategy.add key ~actions:e.actions
          ~probs:(Array.map (fun p -> 2. *. p) e.probs)
          acc)
      Strategy.empty (Strategy.bindings half)
  in
  Alcotest.(check (array rat)) "plan" (S.plan t P1 half) (S.plan t P1 doubled)

let leduc_lp () =
  let t = Lazy.force leduc2 in
  let sol = S.solve t in
  let x, y = sol.plans in
  Alcotest.check rat "equilibrium" Rat.zero
    (Rat.add (S.best_response t P0 y) (S.best_response t P1 x));
  Alcotest.check rat "value" sol.value (S.value t x y);
  let module G = (val leduc2_game) in
  let module C = Cfr.Make (G) in
  let module E = Eval.Make (G) in
  let module B = Best_response.Make (G) in
  let c = C.create ~algorithm:Cfr.Plus () in
  C.run c 500;
  let avg = C.average_strategy c in
  Alcotest.(check bool)
    "cfr+ within its nash conv" true
    (Float.abs (E.expected_value avg -. Rat.to_float sol.value)
    <= B.nash_conv avg)

let exact_zero t (sol : S.solution) =
  let x, y = sol.plans in
  Rat.is_zero (Rat.add (S.best_response t P0 y) (S.best_response t P1 x))

let liars () =
  List.iter
    (fun (dice, faces, wild, n, d) ->
      let config = { Liars_dice.dice; faces; wild } in
      let t = S.build (Liars_dice.game config) in
      let sol = S.solve t in
      Alcotest.check rat (Liars_dice.name config) (Rat.of_ints n d) sol.value;
      Alcotest.(check bool) "nash conv" true (exact_zero t sol))
    [
      (1, 2, false, 0, 1);
      (1, 2, true, 1, 2);
      (1, 3, false, -1, 9);
      (1, 3, true, 1, 9);
      (1, 4, false, -1, 8);
      (2, 2, false, 1, 8);
      (2, 2, true, 7, 8);
    ]

let cold () =
  List.iter
    (fun t ->
      let warm = S.solve t and cold = S.solve ~warm:false t in
      Alcotest.check rat "value" warm.value cold.value;
      Alcotest.(check bool) "cold nash conv" true (exact_zero t cold))
    [ Lazy.force kuhn; Lazy.force leduc2 ]

let liars_wild () =
  let config = { Liars_dice.dice = 1; faces = 4; wild = true } in
  let t = S.build (Liars_dice.game config) in
  let sol = S.solve t in
  Alcotest.check rat "1x4 wild" (Rat.of_ints 1 16) sol.value;
  Alcotest.(check bool) "nash conv" true (exact_zero t sol)

let leduc () =
  let t = S.build (module Leduc) in
  let sol = S.solve t in
  Alcotest.(check (float 1e-6)) "value" (-0.085606) (Rat.to_float sol.value);
  Alcotest.(check bool) "nash conv" true (exact_zero t sol)

module Twice = struct
  type history = int

  let name = "twice"
  let root = 0

  let node = function
    | 0 ->
        Game.Decision
          { player = P0; infoset = "a"; actions = [ ("l", 1); ("r", 2) ] }
    | 1 | 2 ->
        Decision
          { player = P0; infoset = "b"; actions = [ ("l", 3); ("r", 3) ] }
    | _ -> Terminal 1.
end

module Leaky = struct
  type history = int

  let name = "leaky"
  let root = 0

  let node = function
    | 0 -> Game.Chance [ (0.5, 1); (0.4, 1) ]
    | _ -> Terminal 0.
end

let rejects () =
  Alcotest.check_raises "recall"
    (Invalid_argument "Sequence_form.build: imperfect recall") (fun () ->
      ignore (S.build (module Twice)));
  Alcotest.check_raises "chance"
    (Invalid_argument "Sequence_form.build: chance probabilities") (fun () ->
      ignore (S.build (module Leaky)))

let tests =
  [
    Alcotest.test_case "sizes" `Quick sizes;
    Alcotest.test_case "kuhn lp" `Quick kuhn_lp;
    Alcotest.test_case "kuhn family" `Quick family;
    Alcotest.test_case "normalizes weights" `Quick normalizes;
    Alcotest.test_case "kuhn against float" `Quick
      (against_float (module Kuhn) 50);
    Alcotest.test_case "leduc2 against float" `Quick
      (against_float leduc2_game 5);
    Alcotest.test_case "leduc2 lp" `Quick leduc_lp;
    Alcotest.test_case "liar's dice values" `Quick liars;
    Alcotest.test_case "cold start" `Quick cold;
    Alcotest.test_case "leduc value" `Slow leduc;
    Alcotest.test_case "liar's dice 1x4 wild" `Slow liars_wild;
    Alcotest.test_case "rejects" `Quick rejects;
  ]
