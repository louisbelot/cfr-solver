open Cfr_solver

let random_profile rng (tree : Multi_tree.t) =
  let s = ref Strategy.empty in
  Array.iteri
    (fun k key ->
      let n = Array.length tree.labels.(k) in
      let w = Array.init n (fun _ -> Rng.float rng) in
      if Rng.int rng 4 = 0 then w.(Rng.int rng n) <- 0.;
      let total = Array.fold_left ( +. ) 0. w in
      s :=
        Strategy.add key ~actions:tree.labels.(k)
          ~probs:(Array.map (fun x -> x /. total) w)
          !s)
    tree.keys;
  !s

let rollouts = Lbr.[ Own; Uniform; Prefer "c" ]

let check_bound (module G : Game.S) seeds =
  let module M = Multi.Of_game (G) in
  let tree = Multi_tree.build (module M) in
  let module B = Multi_br.Make (M) in
  let rng = Rng.make 7 in
  for _ = 1 to seeds do
    let s = random_profile rng tree in
    for i = 0 to 1 do
      let br = B.value i s in
      List.iter
        (fun rollout ->
          let v, _ = Lbr.respond ~rollout tree i (Multi_br.probs tree s) in
          if v > br +. 1e-12 then
            Alcotest.failf "%s player %d %s: lbr %g above br %g" G.name i
              (Lbr.rollout_name rollout) v br)
        rollouts
    done
  done

let below_best_response () =
  check_bound (module Kuhn) 200;
  check_bound (module Leduc) 20;
  check_bound (Goofspiel.game Goofspiel.standard) 20

let last_mover () =
  let module M = Multi.Of_game (Kuhn) in
  let module B = Multi_br.Make (M) in
  let module L = Lbr.Make (M) in
  let tree = Multi_tree.build (module M) and rng = Rng.make 11 in
  for _ = 1 to 200 do
    let s = random_profile rng tree in
    List.iter
      (fun rollout ->
        Alcotest.(check (float 1e-12))
          "player 1 never moves twice" (B.value 1 s) (L.value ~rollout 1 s))
      rollouts
  done

let best_rollout () =
  List.iter
    (fun (module G : Game.S) ->
      let module M = Multi.Of_game (G) in
      let module B = Multi_br.Make (M) in
      let module L = Lbr.Make (M) in
      let tree = Multi_tree.build (module M) in
      let s = random_profile (Rng.make 3) tree in
      for i = 0 to 1 do
        let rollout = Lbr.Fixed (B.strategy i s) in
        Alcotest.(check (float 1e-12))
          G.name (B.value i s) (L.value ~rollout i s)
      done)
    [ (module Kuhn); (module Leduc); Goofspiel.game Goofspiel.standard ]

let equilibrium () =
  let module M = Multi.Of_game (Kuhn) in
  let module L = Lbr.Make (M) in
  List.iter
    (fun alpha ->
      let s = Kuhn.equilibrium alpha in
      List.iter
        (fun rollout ->
          Array.iter
            (fun g -> Alcotest.(check bool) "no gain" true (g <= 1e-12))
            (L.gains ~rollout s))
        rollouts)
    [ 0.; 0.2; 1. /. 3. ]

let local_choice () =
  let module M = Multi.Of_game (Kuhn) in
  let module L = Lbr.Make (M) in
  let s = L.strategy ~rollout:Uniform 0 Strategy.empty in
  let pick key =
    match Strategy.find s key with
    | Some { actions; probs } -> actions.(if probs.(0) = 1. then 0 else 1)
    | None -> Alcotest.failf "no entry for %s" key
  in
  let s = Alcotest.(check string) in
  s "jack bluffs" "b" (pick "J");
  s "king bets" "b" (pick "K");
  s "jack facing a bet folds" "p" (pick "Jpb");
  s "queen facing a bet calls" "b" (pick "Qpb")

let tests =
  [
    Alcotest.test_case "never above the best response" `Quick
      below_best_response;
    Alcotest.test_case "exact for the last mover" `Quick last_mover;
    Alcotest.test_case "best-response rollout" `Quick best_rollout;
    Alcotest.test_case "no gain at equilibrium" `Quick equilibrium;
    Alcotest.test_case "kuhn choices by hand" `Quick local_choice;
  ]
