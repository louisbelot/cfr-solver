open Cfr_solver
module C = Cfr.Make (Kuhn)
module B = Best_response.Make (Kuhn)

let kuhn ?algorithm ?last ?budget () =
  Session.create ?algorithm ?last ?budget (module Kuhn)

let run s n =
  for _ = 1 to n do
    Session.iterate s
  done

let at (p : Session.point) = p.iteration

let geometric_checkpoints () =
  let s = kuhn () in
  run s 30;
  Alcotest.(check (list int))
    "iterations"
    [ 1; 2; 3; 4; 5; 7; 9; 12; 15; 19; 24; 30 ]
    (List.map at (Session.points s));
  Alcotest.(check (list int))
    "frames"
    (List.map at (Session.points s))
    (Session.frames s)

let checkpoint_at_the_end () =
  let s = kuhn () in
  run s 10;
  Session.checkpoint s;
  Session.checkpoint s;
  Alcotest.(check (list int))
    "iterations"
    [ 1; 2; 3; 4; 5; 7; 9; 10 ]
    (List.map at (Session.points s));
  run s 2;
  Alcotest.(check int)
    "schedule resumes" 12
    (at (List.nth (Session.points s) 8))

let same_as_direct_run () =
  let s = kuhn ~algorithm:Plus () in
  run s 50;
  let c = C.create ~algorithm:Plus () in
  C.run c 38;
  let p = List.find (fun p -> at p = 38) (Session.points s) in
  Alcotest.(check (float 1e-15))
    "average"
    (B.exploitability (C.average_strategy c))
    p.average;
  Alcotest.(check (option (float 1e-15)))
    "current"
    (Some (B.exploitability (C.current_strategy c)))
    p.current;
  C.run c 12;
  let avg = C.average_strategy c in
  Array.iteri
    (fun i (r : Session.row) ->
      let key = (Session.infosets s).(i).key in
      Alcotest.(check (array (float 1e-15)))
        key
        (Strategy.probs avg key ~n:2)
        r.average)
    (Session.table s)

let current_is_regret_matching () =
  let s = kuhn () in
  run s 20;
  Array.iter
    (fun (r : Session.row) ->
      let pos = Array.map (Float.max 0.) r.regret in
      let z = Array.fold_left ( +. ) 0. pos in
      let n = Array.length pos in
      let expected =
        if z > 0. then Array.map (fun x -> x /. z) pos
        else Array.make n (1. /. float_of_int n)
      in
      Alcotest.(check (array (float 1e-12))) "mix" expected r.current)
    (Session.table s)

let thinning_keeps_a_lattice () =
  let s = kuhn ~budget:(3 * 24 * 4) () in
  run s 1000;
  let frames = Session.frames s and points = List.map at (Session.points s) in
  Alcotest.(check bool) "at most four" true (List.length frames <= 4);
  Alcotest.(check int) "first kept" 1 (List.hd frames);
  Alcotest.(check bool)
    "subset" true
    (List.for_all (fun f -> List.mem f points) frames);
  let index = List.mapi (fun i p -> (p, i)) points in
  let rec gaps = function
    | a :: (b :: _ as rest) -> (b - a) :: gaps rest
    | _ -> []
  in
  match gaps (List.map (fun f -> List.assoc f index) frames) with
  | [] -> Alcotest.fail "one frame"
  | g :: rest ->
      Alcotest.(check (list int))
        "even spacing"
        (List.map (fun _ -> g) rest)
        rest

let history_rows () =
  let s = kuhn () in
  run s 100;
  Alcotest.(check bool) "unknown" true (Session.history s "Zz" = None);
  match Session.history s "Qpb" with
  | None -> Alcotest.fail "Qpb missing"
  | Some rows ->
      Alcotest.(check (list int))
        "iterations" (Session.frames s)
        (List.map (fun (r : Session.row) -> r.iteration) rows);
      List.iter
        (fun (r : Session.row) ->
          Alcotest.(check (float 1e-12))
            "sums to one" 1.
            (Array.fold_left ( +. ) 0. r.average))
        rows

let no_last_iterate () =
  let s = kuhn ~last:false () in
  run s 5;
  Alcotest.(check bool)
    "none" true
    (List.for_all
       (fun (p : Session.point) -> p.current = None)
       (Session.points s))

let leduc_converges () =
  let s = Session.create ~algorithm:Plus (module Leduc) in
  for _ = 1 to 200 do
    Session.iterate s
  done;
  let points = Session.points s in
  let first = List.hd points
  and last = List.nth points (List.length points - 1) in
  Alcotest.(check bool) "tenfold" true (last.average < first.average /. 10.)

let tests =
  [
    Alcotest.test_case "geometric checkpoints" `Quick geometric_checkpoints;
    Alcotest.test_case "checkpoint at the end" `Quick checkpoint_at_the_end;
    Alcotest.test_case "same as a direct run" `Quick same_as_direct_run;
    Alcotest.test_case "current is regret matching" `Quick
      current_is_regret_matching;
    Alcotest.test_case "thinning keeps a lattice" `Quick
      thinning_keeps_a_lattice;
    Alcotest.test_case "history rows" `Quick history_rows;
    Alcotest.test_case "no last iterate" `Quick no_last_iterate;
    Alcotest.test_case "leduc converges" `Quick leduc_converges;
  ]
