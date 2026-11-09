open Cfr_solver

module E =
  Meta_lp.Make
    (Lp.Exact)
    (struct
      let delta = Rat.zero
    end)

module A =
  Meta_lp.Make
    (Lp.Approx)
    (struct
      let delta = 1e-9
    end)

let rat = Alcotest.testable Rat.pp Rat.equal

let game rng ~m ~n ~lo ~hi =
  Array.init m (fun _ ->
      Array.init n (fun _ -> Rat.of_int (lo + Rng.int rng (hi - lo + 1))))

let order rng ~m ~n =
  let rec go i j acc =
    if i = m && j = n then List.rev acc
    else if j = n || (i < m && Rng.int rng 2 = 0) then
      go (i + 1) j (`Row i :: acc)
    else go i (j + 1) (`Col j :: acc)
  in
  `Row 0 :: go 1 0 []

let grow add_row add_col conv a steps =
  let m = ref 0 and n = ref 0 in
  List.iter
    (function
      | `Row i ->
          add_row (Array.init !n (fun j -> conv a.(i).(j)));
          incr m
      | `Col j ->
          add_col (Array.init !m (fun i -> conv a.(i).(j)));
          incr n)
    steps

let sum = Array.fold_left Rat.add Rat.zero

let check_optimal a x y v =
  Alcotest.check rat "x sums to one" Rat.one (sum x);
  Alcotest.check rat "y sums to one" Rat.one (sum y);
  Array.iter (fun p -> Alcotest.(check bool) "x >= 0" true (Rat.sign p >= 0)) x;
  Array.iter (fun p -> Alcotest.(check bool) "y >= 0" true (Rat.sign p >= 0)) y;
  let m = Array.length a and n = Array.length a.(0) in
  for j = 0 to n - 1 do
    let u = ref Rat.zero in
    for i = 0 to m - 1 do
      u := Rat.add !u (Rat.mul x.(i) a.(i).(j))
    done;
    Alcotest.(check bool) "row guarantees v" true (Rat.compare !u v >= 0)
  done;
  for i = 0 to m - 1 do
    let u = ref Rat.zero in
    for j = 0 to n - 1 do
      u := Rat.add !u (Rat.mul y.(j) a.(i).(j))
    done;
    Alcotest.(check bool) "column holds v" true (Rat.compare !u v <= 0)
  done

let exact_matches_cold () =
  let rng = Rng.make 11 in
  for trial = 1 to 60 do
    let m = 1 + Rng.int rng 12 and n = 1 + Rng.int rng 12 in
    let lo, hi = if trial mod 2 = 0 then (-1, 1) else (-9, 9) in
    let a = game rng ~m ~n ~lo ~hi in
    let t = E.create ~shift:(Rat.of_int (1 - lo)) in
    grow (E.add_row t) (E.add_col t) Fun.id a (order rng ~m ~n);
    let x, y, v = E.solution t in
    let _, _, v' = Psro.matrix a in
    Alcotest.check rat "value" v' v;
    check_optimal a x y v
  done

let float_matches_exact () =
  let rng = Rng.make 12 in
  for _ = 1 to 40 do
    let m = 1 + Rng.int rng 40 and n = 1 + Rng.int rng 40 in
    let a = game rng ~m ~n ~lo:(-20) ~hi:20 in
    let t = A.create ~shift:21. in
    grow (A.add_row t) (A.add_col t) Rat.to_float a (order rng ~m ~n);
    let x, y, v = A.solution t in
    let _, _, v' = Psro.matrix a in
    Alcotest.(check (float 1e-9)) "value" (Rat.to_float v') v;
    let total = Array.fold_left ( +. ) 0. in
    Alcotest.(check (float 1e-9)) "x" 1. (total x);
    Alcotest.(check (float 1e-9)) "y" 1. (total y)
  done

let rejects () =
  let t = E.create ~shift:Rat.one in
  Alcotest.check_raises "column first"
    (Invalid_argument "Meta_lp.add_col: no rows") (fun () -> E.add_col t [||]);
  E.add_row t [||];
  Alcotest.check_raises "shift" (Invalid_argument "Meta_lp: shift too small")
    (fun () -> E.add_col t [| Rat.of_int (-1) |])

let tests =
  [
    Alcotest.test_case "exact matches cold LP" `Quick exact_matches_cold;
    Alcotest.test_case "float matches exact" `Quick float_matches_exact;
    Alcotest.test_case "rejects bad input" `Quick rejects;
  ]
