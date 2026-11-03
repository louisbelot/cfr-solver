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

let pair hand =
  (Hand.of_string (String.sub hand 0 2), Hand.of_string (String.sub hand 2 2))

let rivers () =
  let board = Hand.parse "Qs Jh 8s 5c" in
  let q = Equity.rivers board in
  List.iter
    (fun hand ->
      let a, b = pair hand in
      let expected =
        List.filter
          (fun r -> not (Array.mem r board || r = a || r = b))
          (List.init 52 Fun.id)
        |> List.map (fun r -> brute (Array.append board [| r |]) (a, b))
        |> List.sort compare |> Array.of_list
      in
      let got = q.(Range.index (a, b)) in
      Alcotest.(check int) hand 46 (Array.length got);
      Array.iteri (fun i x -> close 1e-12 hand x got.(i)) expected)
    [ "AsKs"; "9s7s"; "Td9d"; "5d5h" ];
  Alcotest.(check int) "blocked" 0 (Array.length q.(Range.index (pair "Qs2d")))

let same_distribution_same_ehs2 () =
  let board = Hand.parse "Qs Jh 8d 5c" in
  let q = Equity.rivers board and v = Equity.ehs2 board in
  let seen = Hashtbl.create 128 in
  Array.iteri
    (fun i x ->
      if x <> [||] then
        match Hashtbl.find_opt seen x with
        | Some e when e <> v.(i) -> Alcotest.fail "equal rivers, different ehs2"
        | Some _ -> ()
        | None -> Hashtbl.add seen x v.(i))
    q;
  Alcotest.(check int) "distinct" 86 (Hashtbl.length seen)

let quantized () =
  let board = Hand.parse "Kd Tc 6s 4h" in
  Array.iter
    (Array.iter (fun x -> close 1e-12 "one cluster" 0.5 x))
    (Equity.rivers ~quantize:1 board);
  let q = Equity.rivers ~quantize:3 board in
  let values = Hashtbl.create 64 in
  Array.iter (Array.iter (fun x -> Hashtbl.replace values x ())) q;
  if Hashtbl.length values > 3 * 48 then Alcotest.fail "more than 3 per river"

let histogram () =
  let board = Hand.parse "Qs Jh 8s 5c" in
  let h = Equity.histogram ~bins:10 board and q = Equity.rivers board in
  Array.iteri
    (fun i x ->
      if q.(i) = [||] then Alcotest.(check int) "blocked" 0 (Array.length x)
      else begin
        close 1e-12 "mass" 1. (Array.fold_left ( +. ) 0. x);
        let top =
          Array.fold_left (fun n v -> if v >= 0.9 then n + 1 else n) 0 q.(i)
        in
        close 1e-12 "top bin" (float_of_int top /. 46.) x.(9)
      end)
    h

let tests =
  [
    Alcotest.test_case "against brute force" `Quick against_brute_force;
    Alcotest.test_case "mean is one half" `Quick mean_half;
    Alcotest.test_case "extremes" `Quick extremes;
    Alcotest.test_case "ehs2" `Quick ehs2;
    Alcotest.test_case "river distributions" `Quick rivers;
    Alcotest.test_case "equal distributions, equal ehs2" `Quick
      same_distribution_same_ehs2;
    Alcotest.test_case "quantized rivers" `Quick quantized;
    Alcotest.test_case "histogram" `Quick histogram;
  ]
