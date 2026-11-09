open Cfr_solver

let random_profile rng (tree : Multi_tree.t) skip =
  Array.fold_left
    (fun (s, k) key ->
      if tree.owner.(k) = skip then (s, k + 1)
      else
        let n = Array.length tree.labels.(k) in
        let w = Array.init n (fun _ -> Rng.float rng) in
        let t = Array.fold_left ( +. ) 0. w in
        ( Strategy.add key ~actions:tree.labels.(k)
            ~probs:(Array.map (fun x -> x /. t) w)
            s,
          k + 1 ))
    (Strategy.empty, 0) tree.keys
  |> fst

let cfr_oracle () =
  let rng = Rng.make 9 in
  List.iter
    (fun (name, g) ->
      let tree = Multi_tree.build g in
      for p = 0 to tree.players - 1 do
        let others = random_profile rng tree p in
        let best, _ = Multi_br.best tree p others in
        let value k =
          let s = Cfr_br.respond tree p others ~iterations:k in
          (Multi_br.values tree (Strategy.union s others)).(p)
        in
        let v10 = value 10 and v2000 = value 2000 in
        Alcotest.(check bool)
          (name ^ " below best") true
          (v2000 <= best +. 1e-12);
        Alcotest.(check bool) (name ^ " close") true (best -. v2000 < 1e-3);
        Alcotest.(check bool) (name ^ " improves") true (v10 <= v2000 +. 1e-12)
      done)
    [
      ("kuhn", (module Multi.Of_game (Kuhn) : Multi.S));
      ("leduc", (module Multi.Of_game (Leduc)));
      ("kuhn3", (module (val Kuhn_n.game 3)));
    ]

let tests =
  [ Alcotest.test_case "approaches the best response" `Quick cfr_oracle ]
