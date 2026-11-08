open Cfr_solver

let random_profile (module G : Multi.S) seed =
  let module E = Multi.Eval (G) in
  let rng = Rng.make seed in
  List.fold_left
    (fun s p ->
      List.fold_left
        (fun s (key, actions) ->
          let w =
            Array.map
              (fun _ -> if Rng.float rng < 0.2 then 0. else Rng.float rng)
              actions
          in
          Strategy.add key ~actions ~probs:(Regret_matching.normalize w) s)
        s (E.infosets p))
    Strategy.empty
    (List.init G.players Fun.id)

let agrees (module G : Multi.S) seeds () =
  let module B = Multi_br.Make (G) in
  let module R = Multi_br_ref.Make (G) in
  let module E = Multi.Eval (G) in
  let tree = Multi_tree.build (module G) in
  List.iter
    (fun seed ->
      let sigma = random_profile (module G) seed in
      let u = E.values sigma and u' = Multi_br.values tree sigma in
      Array.iteri
        (fun i x ->
          if Float.abs (x -. u'.(i)) > 1e-12 then
            Alcotest.failf "value %d: %g vs %g" i x u'.(i))
        u;
      Array.iteri
        (fun i g ->
          let g' = (B.gains sigma).(i) in
          if Float.abs (g -. g') > 1e-12 then
            Alcotest.failf "gain %d: %.15g vs %.15g" i g g')
        (R.gains sigma);
      for p = 0 to G.players - 1 do
        let v, s = Multi_br.best tree p sigma in
        let earned = (E.values (Strategy.union s sigma)).(p) in
        if Float.abs (v -. earned) > 1e-12 then
          Alcotest.failf "player %d earns %g, claims %g" p earned v
      done)
    seeds

let tests =
  [
    Alcotest.test_case "kuhn3" `Quick
      (agrees (module (val Kuhn_n.game 3)) (List.init 20 Fun.id));
    Alcotest.test_case "kuhn4" `Quick
      (agrees (module (val Kuhn_n.game 4)) (List.init 5 Fun.id));
    Alcotest.test_case "leduc" `Quick
      (agrees (module Multi.Of_game (Leduc)) [ 1; 2; 3 ]);
    Alcotest.test_case "leduc3-6" `Quick
      (agrees (module (val Leduc_n.game ~ranks:3 3)) [ 1; 2 ]);
  ]
