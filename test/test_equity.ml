open Cfr_solver

let brute board (a, b) =
  let mine = Hand.eval (Array.append [| a; b |] board) in
  let score = ref 0. and count = ref 0 in
  Array.iter
    (fun (c, d) ->
      if not (Range.blocks board (c, d) || c = a || c = b || d = a || d = b)
      then begin
        let theirs = Hand.eval (Array.append [| c; d |] board) in
        incr count;
        score :=
          !score
          +. if mine > theirs then 1. else if mine = theirs then 0.5 else 0.
      end)
    Range.combos;
  !score /. float_of_int !count

let close tol name x y =
  if not (Float.abs (x -. y) <= tol) then
    Alcotest.failf "%s: expected %.15g, got %.15g" name x y

let against_brute_force () =
  List.iter
    (fun board ->
      let board = Hand.parse board in
      let v = Equity.hs board in
      Array.iteri
        (fun i c ->
          if Range.blocks board c then
            begin if not (Float.is_nan v.(i)) then Alcotest.fail "blocked combo"
            end
          else if i mod 7 = 0 then
            close 1e-12 (Range.combo_to_string c) (brute board c) v.(i))
        Range.combos)
    [ "Qs Jh 8d 5c 2s"; "Ah Ad 7c 7s 2h"; "9h 8h 7h 6h 2c" ]

let mean_half () =
  let board = Hand.parse "Kd Tc 6s 4h 3h" in
  let v = Equity.hs board in
  let live = List.filter (fun x -> not (Float.is_nan x)) (Array.to_list v) in
  Alcotest.(check int) "live" 1081 (List.length live);
  close 1e-12 "mean" 0.5
    (List.fold_left ( +. ) 0. live /. float_of_int (List.length live))

let extremes () =
  let v = Equity.hs (Hand.parse "As Ks Qs Js 2d") in
  close 0. "royal" 1. v.(Range.index (Hand.of_string "Ts", Hand.of_string "3c"));
  let v = Equity.hs (Hand.parse "As Ks Qs Js Ts") in
  Array.iter (fun x -> if not (Float.is_nan x) then close 0. "board" 0.5 x) v

let ehs2 () =
  let board = Hand.parse "Qs Jh 8d 5c" in
  let v = Equity.ehs2 board in
  List.iter
    (fun hand ->
      let a, b =
        ( Hand.of_string (String.sub hand 0 2),
          Hand.of_string (String.sub hand 2 2) )
      in
      let rivers =
        List.filter
          (fun r -> not (Array.mem r board || r = a || r = b))
          (List.init 52 Fun.id)
      in
      let expected =
        List.fold_left
          (fun acc r ->
            let x = brute (Array.append board [| r |]) (a, b) in
            acc +. (x *. x))
          0. rivers
        /. float_of_int (List.length rivers)
      in
      close 1e-12 hand expected v.(Range.index (a, b)))
    [ "AsKs"; "Th9h"; "7c2d"; "QdQc" ]

let tests =
  [
    Alcotest.test_case "against brute force" `Quick against_brute_force;
    Alcotest.test_case "mean is one half" `Quick mean_half;
    Alcotest.test_case "extremes" `Quick extremes;
    Alcotest.test_case "ehs2" `Quick ehs2;
  ]
