open Cfr_solver

let trees = Hashtbl.create 4

let kuhn ranks =
  match Hashtbl.find_opt trees ranks with
  | Some t -> t
  | None ->
      let t = Multi_tree.build (module (val Kuhn_n.game ~ranks 3)) in
      Hashtbl.add trees ranks t;
      t

let solved = Hashtbl.create 8

let solution ranks o =
  match Hashtbl.find_opt solved (ranks, o) with
  | Some s -> s
  | None ->
      let tree = kuhn ranks in
      let t = Team.build tree ~adversary:o in
      let s = (t, Team.solve t) in
      Hashtbl.add solved (ranks, o) s;
      s

let random_adversary t (tree : Multi_tree.t) o seed =
  let rng = Rng.make seed and sigma = ref Strategy.empty in
  Array.iteri
    (fun k key ->
      if tree.owner.(k) = o then begin
        let actions = tree.labels.(k) in
        let w = Array.map (fun _ -> Rng.float rng) actions in
        sigma :=
          Strategy.add key ~actions ~probs:(Regret_matching.normalize w) !sigma
      end)
    tree.keys;
  (!sigma, Team.realization t !sigma)

let published ranks values () =
  List.iter
    (fun (o, v) ->
      let t, s = solution ranks o in
      Alcotest.(check bool) "proved optimal" true s.optimal;
      Alcotest.(check (float 5e-5)) "farina table 2" v (Rat.to_float s.value);
      let up, _, _ = Team.respond t s.response in
      Alcotest.(check (float 1e-9)) "certified" (Rat.to_float s.value) up)
    values

let last_seat () =
  let _, s = solution 4 2 in
  Alcotest.(check string) "minus 1/24" "-1/24" (Rat.to_string s.value)

let three_ranks () =
  List.iter
    (fun o ->
      let _, s = solution 3 o in
      Alcotest.(check string) "zero" "0" (Rat.to_string s.value))
    [ 0; 1; 2 ]

let certificate () =
  List.iter
    (fun o ->
      let t, s = solution 4 o in
      Alcotest.(check string)
        "best response to the mixture" (Rat.to_string s.value)
        (Rat.to_string (Team.guarantee t s.support));
      let total =
        List.fold_left (fun acc (l, _) -> Rat.add acc l) Rat.zero s.support
      in
      Alcotest.(check string) "distribution" "1" (Rat.to_string total);
      let up, _, exact = Team.respond t s.response in
      Alcotest.(check bool) "exact" true exact;
      Alcotest.(check (float 1e-9)) "no better column" (Rat.to_float s.value) up)
    [ 0; 1; 2 ]

let columns () =
  let tree = kuhn 4 in
  List.iter
    (fun o ->
      let t, s = solution 4 o in
      let a, b = Team.members t in
      List.iteri
        (fun i (_, plan) ->
          let sigma, y = random_adversary t tree o i in
          let profile = Strategy.union (Team.strategies t plan) sigma in
          let u = Multi_br.values tree profile in
          let c = Team.column t plan in
          let v = ref 0. in
          Array.iteri (fun j x -> v := !v +. (Rat.to_float x *. y.(j))) c;
          Alcotest.(check (float 1e-12)) "team payoff" (u.(a) +. u.(b)) !v)
        s.support)
    [ 0; 1; 2 ]

let brute_force () =
  let tree = kuhn 3 in
  let t = Team.build tree ~adversary:2 in
  let na, nb = Team.local_plans t in
  let rec all sizes =
    match sizes with
    | [] -> [ [] ]
    | n :: rest ->
        let tail = all rest in
        List.concat_map
          (fun l -> List.map (List.cons l) tail)
          (List.init n Fun.id)
  in
  let pas = List.map Array.of_list (all (Array.to_list na))
  and pbs = List.map Array.of_list (all (Array.to_list nb)) in
  List.iter
    (fun seed ->
      let _, y = random_adversary t tree 2 seed in
      let value plan =
        let c = Team.column t { Team.pa = plan.(0); pb = plan.(1) } in
        let v = ref 0. in
        Array.iteri (fun j x -> v := !v +. (Rat.to_float x *. y.(j))) c;
        !v
      in
      let best =
        List.fold_left
          (fun m pa ->
            List.fold_left (fun m pb -> Float.max m (value [| pa; pb |])) m pbs)
          Float.neg_infinity pas
      in
      let v, plan, exact = Team.respond t y in
      Alcotest.(check bool) "exact" true exact;
      Alcotest.(check (float 1e-12)) "max" best v;
      Alcotest.(check (float 1e-12))
        "plan value" best
        (value [| plan.pa; plan.pb |]))
    [ 1; 2; 3 ]

let beats_nash () =
  let module C = Multi_cfr.Make ((val Kuhn_n.game 3)) in
  let c = C.create ~algorithm:Cfr.dcfr () in
  C.run c 2000;
  let nash = C.average_strategy c and tree = kuhn 4 in
  List.iter
    (fun o ->
      let t, s = solution 4 o in
      let value = Rat.to_float s.value in
      let independent =
        -.fst (Multi_br.best tree o (Team.marginals t s.support))
      in
      let at_nash = -.fst (Multi_br.best tree o nash) in
      Alcotest.(check bool) "marginals" true (independent <= value +. 1e-12);
      Alcotest.(check bool) "nash" true (at_nash <= value +. 1e-6);
      Alcotest.(check bool) "strictly" true (value -. at_nash > 0.004))
    [ 0; 1; 2 ]

let rejects () =
  let two = Multi_tree.build (module (val Kuhn_n.game 2)) in
  Alcotest.check_raises "players" (Invalid_argument "Team.build: three players")
    (fun () -> ignore (Team.build two ~adversary:0))

let tests =
  [
    Alcotest.test_case "farina et al. 4 ranks" `Quick
      (published 4 [ (0, 0.0379); (1, 0.0265); (2, -0.0417) ]);
    Alcotest.test_case "farina et al. 12 ranks" `Slow
      (published 12 [ (0, 0.0664); (1, 0.0380); (2, -0.0140) ]);
    Alcotest.test_case "last seat is -1/24" `Quick last_seat;
    Alcotest.test_case "3 ranks is zero" `Quick three_ranks;
    Alcotest.test_case "certificate" `Quick certificate;
    Alcotest.test_case "columns match the tree" `Quick columns;
    Alcotest.test_case "oracle against brute force" `Slow brute_force;
    Alcotest.test_case "beats the nash family" `Quick beats_nash;
    Alcotest.test_case "three players only" `Quick rejects;
  ]
