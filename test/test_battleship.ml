open Cfr_solver

let close ?(eps = 1e-9) what a b =
  if Float.abs (a -. b) > eps then Alcotest.failf "%s: %.12g vs %.12g" what a b

let ship length = { Battleship.length; value = 1. }

let config ?(ships = [ 1 ]) ?(repeat = false) ?(gamma = 2.) height width shots =
  {
    Battleship.height;
    width;
    ships = List.map ship ships;
    shots;
    repeat;
    gamma;
  }

let tree c = Multi_tree.build (Battleship.game c)

let rec leaves f = function
  | Multi_tree.Leaf u -> f u
  | Decision (_, ch) | Chance (_, ch) -> Array.iter (leaves f) ch

let sizes (t : Multi_tree.t) =
  let seqs = [| 1; 1 |] and infos = [| 0; 0 |] and n = ref 0 in
  Array.iteri
    (fun k p ->
      infos.(p) <- infos.(p) + 1;
      seqs.(p) <- seqs.(p) + Array.length t.labels.(k))
    t.owner;
  leaves (fun _ -> incr n) t.root;
  (seqs, infos, !n)

let open_spiel () =
  List.iter
    (fun (h, w, shots, ships, s0, s1, i0, i1, z) ->
      let seqs, infos, n = sizes (tree (config ~ships h w shots)) in
      let name = Printf.sprintf "%dx%d/%d" h w shots in
      Alcotest.(check (array int)) (name ^ " sequences") [| s0; s1 |] seqs;
      Alcotest.(check (array int)) (name ^ " infosets") [| i0; i1 |] infos;
      Alcotest.(check int) (name ^ " terminals") z n)
    [
      (2, 2, 2, [ 1 ], 165, 341, 53, 109, 1072);
      (2, 2, 3, [ 1 ], 741, 917, 341, 397, 2224);
      (2, 2, 2, [ 1; 2 ], 1197, 3597, 397, 1189, 9216);
      (2, 2, 3, [ 1; 2 ], 13485, 22029, 6541, 10405, 32256);
      (2, 3, 2, [ 1 ], 943, 3787, 187, 751, 19116);
      (2, 3, 3, [ 1 ], 15343, 46987, 3787, 11551, 191916);
    ]

let relevant_pairs () =
  Alcotest.(check int)
    "pairs" 35241
    (Efce_lp.pairs (Efce_lp.plan (tree (config 2 2 3))))

let placements () =
  let count c l = List.length (Battleship.placements c l) in
  Alcotest.(check int) "single cells" 4 (count (config 2 2 1) 1);
  Alcotest.(check int) "dominoes on 2x3" 7 (count (config 2 3 1) 2);
  Alcotest.(check int) "too long" 0 (count (config 2 3 1) 4);
  let masks = List.map snd (Battleship.placements (config 2 3 1) 2) in
  Alcotest.(check (list int)) "masks" [ 3; 6; 24; 48; 9; 18; 36 ] masks

let feasible_only () =
  let module G = (val Battleship.game (config ~ships:[ 1; 2 ] 1 3 3)) in
  match G.node G.root with
  | Decision { actions; _ } ->
      Alcotest.(check (list string))
        "ends only" [ "h:a1"; "h:a3" ] (List.map fst actions)
  | _ -> Alcotest.fail "root"

let first_sink_ends () =
  let t = tree (config 1 1 1) in
  let seen = ref [] in
  leaves (fun u -> seen := Array.to_list u :: !seen) t.root;
  Alcotest.(check (list (list (float 0.)))) "one leaf" [ [ 1.; -2. ] ] !seen

let payoffs () =
  let seen gamma =
    let t = tree (config ~ships:[ 1; 2 ] ~gamma 2 2 3) in
    let acc = ref [] in
    leaves
      (fun u ->
        if not (List.mem (u.(0), u.(1)) !acc) then acc := (u.(0), u.(1)) :: !acc)
      t.root;
    List.sort compare !acc
  in
  List.iter (fun (a, b) -> close "zero sum" 0. (a +. b)) (seen 1.);
  Alcotest.(check (list (pair (float 0.) (float 0.))))
    "outcomes"
    [ (-3., 0.); (-1., -1.); (0., -3.); (2., -4.) ]
    (seen 2.)

let outcomes () =
  let t = tree (config ~ships:[ 2 ] 1 2 2) in
  let has s =
    Array.exists
      (fun k ->
        let n = String.length s and m = String.length k in
        let rec go i = i + n <= m && (String.sub k i n = s || go (i + 1)) in
        go 0)
      t.keys
  in
  Alcotest.(check bool) "hit" true (has "/a1:H");
  Alcotest.(check bool) "no water" false (has ":W");
  Alcotest.(check bool) "opponent shot" true (has "/xa1")

let repeats () =
  Alcotest.check_raises "too many shots"
    (Invalid_argument "Battleship.game: config") (fun () ->
      ignore (Battleship.game (config 1 2 3)));
  Alcotest.check_raises "no room"
    (Invalid_argument "Battleship.game: ships do not fit") (fun () ->
      ignore (Battleship.game (config ~ships:[ 2; 2 ] 1 3 1)));
  let _, _, n = sizes (tree (config ~repeat:true 1 2 3)) in
  Alcotest.(check bool) "repeated shots" true (n > 0)

let farina_example () =
  let plan = Efce_lp.plan (tree (config 3 1 2)) in
  let lp = Option.get (Efce_lp.solve ~exact:true plan) in
  close "player 1" (-1. /. 9.) lp.values.(0);
  close "player 2" (-11. /. 18.) lp.values.(1);
  let eps = 1e-3 in
  let g = Optimal.create plan in
  let relaxed = Option.get (Efce_lp.solve ~exact:true ~slack:eps plan) in
  let best, _ = Optimal.bisect ~eps g in
  let w = Optimal.welfare g best.xi in
  if
    w < (-13. /. 18.) -. (2. *. eps)
    || w > relaxed.values.(0) +. relaxed.values.(1) +. 1e-9
  then Alcotest.failf "learned welfare %g" w;
  if Optimal.gain g best.xi > eps then
    Alcotest.failf "gain %g" (Optimal.gain g best.xi)

let tests =
  [
    Alcotest.test_case "open spiel sizes" `Quick open_spiel;
    Alcotest.test_case "relevant pairs" `Quick relevant_pairs;
    Alcotest.test_case "placements" `Quick placements;
    Alcotest.test_case "feasible only" `Quick feasible_only;
    Alcotest.test_case "first sink ends" `Quick first_sink_ends;
    Alcotest.test_case "payoffs" `Quick payoffs;
    Alcotest.test_case "outcomes" `Quick outcomes;
    Alcotest.test_case "repeats" `Quick repeats;
    Alcotest.test_case "farina example" `Quick farina_example;
  ]
