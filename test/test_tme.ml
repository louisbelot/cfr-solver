open Cfr_solver

module Guess = struct
  type history = int list

  let name = "guess"
  let players = 3
  let root = []

  let node = function
    | [ a; b; g ] ->
        Multi.Terminal
          (if a = b && a <> g then [| 0.5; 0.5; -1. |] else [| 0.; 0.; 0. |])
    | h ->
        let p = List.length h in
        Multi.Decision
          {
            player = p;
            infoset = Printf.sprintf "p%d" p;
            actions = [ ("0", h @ [ 0 ]); ("1", h @ [ 1 ]) ];
          }
end

let rat = Alcotest.testable Rat.pp Rat.equal
let kuhn ranks = Multi_tree.build (module (val Kuhn_n.game ~ranks 3))
let solved = Hashtbl.create 4

let solution ranks o =
  match Hashtbl.find_opt solved (ranks, o) with
  | Some s -> s
  | None ->
      let t = Team.build (kuhn ranks) ~adversary:o in
      let s = (t, Tme.solve t) in
      Hashtbl.add solved (ranks, o) s;
      s

let plan_ok (p : Team.plex) r =
  Rat.equal r.(0) Rat.one
  && Array.for_all (fun v -> Rat.sign v >= 0) r
  && Array.for_all
       (fun (q, f, n) ->
         let sum = ref Rat.zero in
         for a = 0 to n - 1 do
           sum := Rat.add !sum r.(f + a)
         done;
         Rat.equal !sum r.(q))
       p.infos

let random_plan rng (p : Team.plex) =
  let r = Array.make p.size Rat.zero in
  r.(0) <- Rat.one;
  Array.to_list p.infos
  |> List.sort (fun (_, f, _) (_, f', _) -> compare f f')
  |> List.iter (fun (q, f, n) ->
      let w = Array.init n (fun _ -> 1 + Rng.int rng 9) in
      let total = Array.fold_left ( + ) 0 w in
      Array.iteri
        (fun a wa -> r.(f + a) <- Rat.mul r.(q) (Rat.of_ints wa total))
        w);
  r

let guess () =
  let tree = Multi_tree.build (module Guess) in
  let t = Team.build tree ~adversary:2 in
  let s = Tme.solve t in
  Alcotest.check rat "tme" (Rat.of_ints 1 4) s.lower;
  Alcotest.(check (float 1e-6)) "upper" 0.25 s.upper;
  Alcotest.(check (float 1e-9)) "root is the correlated value" 0.5 s.root;
  Alcotest.check rat "tmecor" (Rat.of_ints 1 2) (Team.solve t).value

let certificate () =
  List.iter
    (fun o ->
      let t, s = solution 4 o in
      let pa, pb, _ = Team.plexes t in
      Alcotest.(check bool) "x is a plan" true (plan_ok pa s.x);
      Alcotest.(check bool) "z is a plan" true (plan_ok pb s.z);
      Alcotest.check rat "guarantee" s.lower (Team.guard t s.x s.z))
    [ 0; 2 ]

let bracket () =
  List.iter
    (fun (o, tmecor) ->
      let t, s = solution 4 o in
      let lo = Rat.to_float s.lower in
      Alcotest.(check bool) "gap closed" true (s.upper -. lo <= 1e-6);
      Alcotest.(check bool) "below tmecor" true (s.upper <= tmecor +. 1e-9);
      Alcotest.(check bool) "root above tmecor" true (s.root >= tmecor -. 1e-9);
      let rng = Rng.make (17 + o) in
      let pa, pb, _ = Team.plexes t in
      for _ = 1 to 25 do
        let g = Team.guard t (random_plan rng pa) (random_plan rng pb) in
        Alcotest.(check bool)
          "random pair" true
          (Rat.to_float g <= s.upper +. 1e-9)
      done)
    [ (0, 5. /. 132.); (2, -1. /. 24.) ]

let values () =
  Alcotest.check rat "adversary first" (Rat.of_ints 7 192)
    (snd (solution 4 0)).lower;
  Alcotest.check rat "adversary third" (Rat.of_ints (-1) 24)
    (snd (solution 4 2)).lower

let nash () =
  let tree = kuhn 4 in
  let (module G) = Kuhn_n.game ~ranks:4 3 in
  let module C = Multi_cfr.Make (G) in
  let c = C.create ~algorithm:Cfr.dcfr () in
  C.run c 2000;
  let sigma = C.average_strategy c in
  let members =
    List.fold_left
      (fun acc (key, (e : Strategy.entry)) ->
        match Array.find_index (String.equal key) tree.keys with
        | Some k when tree.owner.(k) <> 0 ->
            Strategy.add key ~actions:e.actions ~probs:e.probs acc
        | _ -> acc)
      Strategy.empty (Strategy.bindings sigma)
  in
  let br, _ = Multi_br.best tree 0 members in
  let _, s = solution 4 0 in
  Alcotest.(check bool) "beats nash" true (-.br <= Rat.to_float s.lower +. 1e-6)

let middle_seat () =
  let t = Team.build (kuhn 4) ~adversary:1 in
  let s = Tme.solve ~limit:25 t in
  let attained =
    Rat.make
      (Bigint.of_string "144281031973174991")
      (Bigint.of_string "6092627077201133568")
  in
  Alcotest.check rat "guarantee" s.lower (Team.guard t s.x s.z);
  Alcotest.(check bool)
    "upper stays valid" true
    (s.upper >= Rat.to_float attained -. 1e-12);
  Alcotest.(check bool) "ordered" true (Rat.to_float s.lower <= s.upper);
  Alcotest.(check bool) "below tmecor" true (s.upper <= (7. /. 264.) +. 1e-9);
  Alcotest.(check bool) "children warm" true (s.warm >= s.nodes - 2)

let branching () =
  let t = Team.build (kuhn 4) ~adversary:0 in
  List.iter
    (fun strong ->
      let s = Tme.solve ~strong t in
      let lo = Rat.to_float s.lower in
      Alcotest.(check bool) "tme" true (Float.abs (lo -. (7. /. 192.)) <= 1e-6);
      Alcotest.(check bool) "upper" true (s.upper >= (7. /. 192.) -. 1e-12);
      Alcotest.check rat "guarantee" s.lower (Team.guard t s.x s.z))
    [ 1; 2; 8 ]

let three_ranks () =
  List.iter
    (fun o ->
      let _, s = solution 3 o in
      Alcotest.check rat "zero" Rat.zero s.lower)
    [ 0; 1; 2 ]

let tests =
  [
    Alcotest.test_case "guessing game" `Quick guess;
    Alcotest.test_case "certificate" `Quick certificate;
    Alcotest.test_case "bounds" `Quick bracket;
    Alcotest.test_case "kuhn values" `Quick values;
    Alcotest.test_case "above nash" `Quick nash;
    Alcotest.test_case "middle seat, truncated" `Quick middle_seat;
    Alcotest.test_case "strong branching" `Quick branching;
    Alcotest.test_case "three ranks" `Quick three_ranks;
  ]
