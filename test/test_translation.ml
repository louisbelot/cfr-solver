open Cfr_solver

let close tol name expected got =
  if Float.abs (expected -. got) > tol then
    Alcotest.failf "%s: expected %.12g, got %.12g" name expected got

let grid = List.init 41 (fun i -> 0.5 +. (0.0125 *. float_of_int i))

let endpoints () =
  List.iter
    (fun m ->
      let f = Translation.low m ~a:0.5 ~b:1. in
      close 1e-15 (Translation.name m ^ " at a") 1. (f 0.5);
      close 1e-15 (Translation.name m ^ " at b") 0. (f 1.);
      ignore
        (List.fold_left
           (fun prev x ->
             let y = f x in
             if y > prev +. 1e-15 || y < 0. || y > 1. then
               Alcotest.failf "%s not decreasing at %g" (Translation.name m) x;
             y)
           1. grid))
    Translation.all;
  Alcotest.check_raises "outside" (Invalid_argument "Translation.low")
    (fun () -> ignore (Translation.low Arithmetic ~a:0.5 ~b:1. 1.5))

let calling_frequency () =
  List.iter
    (fun (a, b) ->
      List.iter
        (fun t ->
          let x = a +. (t *. (b -. a)) in
          let f = Translation.low Pseudo_harmonic ~a ~b x in
          close 1e-12
            (Printf.sprintf "a=%g b=%g x=%g" a b x)
            (1. /. (1. +. x))
            ((f /. (1. +. a)) +. ((1. -. f) /. (1. +. b))))
        [ 0.; 0.1; 0.37; 0.5; 0.9; 1. ])
    [ (0.25, 0.5); (0.5, 1.); (1., 2.); (0.33, 3.) ]

let nearest () =
  let f = Translation.low Nearest ~a:0.5 ~b:1. in
  Alcotest.(check (list (float 0.)))
    "steps" [ 1.; 1.; 0.; 0. ]
    (List.map f [ 0.6; 0.75; 0.76; 0.9 ])

let street sizes =
  {
    Bet_tree.bets = List.map (fun f -> Bet_tree.Pot f) sizes;
    raises = [ Pot 1. ];
    cap = 3;
  }

let config sizes =
  { Bet_tree.pot = 10.; stack = 200.; streets = [ street sizes ] }

let board = Hand.parse "Qs Jh 8d 5c 2s"
let r0 = Range.of_string "22+, A2s+, KTs+, QTs+, JTs, T9s, 98s, ATo+, KJo+"
let r1 = Range.of_string "22+, A2+, K9+, Q9+, J9+, T9, 98, 87s, 76s"

let solve sizes n =
  let t = Subgame.create ~algorithm:Cfr.dcfr (config sizes) ~board r0 r1 in
  Subgame.run t n;
  t

let abstract = lazy (solve [ 0.5; 1. ] 60)
let full = lazy (Subgame.create (config [ 0.5; 0.75; 1. ]) ~board r0 r1)
let pseudo = Translation.low Pseudo_harmonic

let identity () =
  let t = Lazy.force abstract in
  let prof = Subgame.average t in
  let back = Subgame.translate ~low:pseudo ~abstract:t prof t in
  Array.iteri
    (fun i node ->
      Array.iteri
        (fun a probs ->
          Array.iteri
            (fun h p ->
              close 1e-12 (Printf.sprintf "%d %d %d" i a h) p back.(i).(a).(h))
            probs)
        node)
    prof

let extra_size () =
  let t = Lazy.force abstract and g = Lazy.force full in
  let prof = Subgame.average t in
  let lifted = Subgame.translate ~low:pseudo ~abstract:t prof g in
  close 1e-9 "value" (Subgame.value t prof) (Subgame.value g lifted);
  List.iter
    (fun p ->
      let gain =
        Subgame.best_response g p lifted -. Subgame.best_response t p prof
      in
      if gain < -1e-9 then Alcotest.failf "player %d lost %g" p gain)
    [ 0; 1 ]

let linear () =
  let t = Lazy.force abstract and g = Lazy.force full in
  let prof = Subgame.average t in
  let attacker =
    Subgame.create ~algorithm:Cfr.Plus (config [ 0.5; 0.75; 1. ]) ~board r0 r1
  in
  Subgame.run attacker 20;
  let att = Subgame.average attacker in
  let value f =
    let defender =
      Subgame.translate ~low:(fun ~a:_ ~b:_ _ -> f) ~abstract:t prof g
    in
    Subgame.value g (Subgame.combine g att defender)
  in
  let v0 = value 0. and v1 = value 1. in
  if Float.abs (v1 -. v0) < 1e-3 then
    Alcotest.failf "mappings agree: %g %g" v0 v1;
  List.iter
    (fun f ->
      close 1e-9 (Printf.sprintf "f=%g" f)
        ((f *. v1) +. ((1. -. f) *. v0))
        (value f))
    [ 0.3; 0.8 ]

let invalid () =
  let t = Lazy.force abstract in
  let prof = Subgame.average t in
  let fails name g =
    match Subgame.translate ~low:pseudo ~abstract:t prof g with
    | _ -> Alcotest.failf "%s accepted" name
    | exception Invalid_argument _ -> ()
  in
  fails "ranges" (Subgame.create (config [ 0.5; 1. ]) ~board r1 r0);
  fails "missing size" (Subgame.create (config [ 0.75; 1. ]) ~board r0 r1)

let tests =
  [
    Alcotest.test_case "endpoints" `Quick endpoints;
    Alcotest.test_case "calling frequency" `Quick calling_frequency;
    Alcotest.test_case "nearest" `Quick nearest;
    Alcotest.test_case "identity" `Quick identity;
    Alcotest.test_case "extra size" `Quick extra_size;
    Alcotest.test_case "linear in the mapping" `Quick linear;
    Alcotest.test_case "invalid" `Quick invalid;
  ]
