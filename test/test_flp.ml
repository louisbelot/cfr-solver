open Cfr_solver

let prop ~count name gen f =
  Alcotest.test_case name `Quick (fun () ->
      QCheck2.Test.check_exn (QCheck2.Test.make ~count ~name gen f))

let q = Rat.of_ints
let i = Rat.of_int
let row coeffs rel rhs = { Lp.coeffs; rel; rhs }

let exact ~vars obj rows =
  match Lp.maximize ~vars obj rows with
  | Lp.Optimal { value; _ } -> Some (Rat.to_float value)
  | Infeasible -> None
  | Unbounded -> Alcotest.fail "unbounded"

let close a b = Float.abs (a -. b) < 1e-7

let wyndor () =
  let t, s =
    Flp.solve ~vars:2
      [ (0, i 3); (1, i 5) ]
      [
        row [ (0, i 1) ] Le (i 4);
        row [ (1, i 2) ] Le (i 12);
        row [ (0, i 3); (1, i 2) ] Le (i 18);
      ]
  in
  Alcotest.(check bool) "optimal" true (s = Flp.Optimal);
  Alcotest.(check (float 1e-9)) "value" 36. (Flp.value t);
  Alcotest.(check (array (float 1e-9))) "point" [| 2.; 6. |] (Flp.x t)

let beale () =
  let t, s =
    Flp.solve ~vars:4
      [ (0, q 3 4); (1, i (-20)); (2, q 1 2); (3, i (-6)) ]
      [
        row [ (0, q 1 4); (1, i (-8)); (2, i (-1)); (3, i 9) ] Le (i 0);
        row [ (0, q 1 2); (1, i (-12)); (2, q (-1) 2); (3, i 3) ] Le (i 0);
        row [ (2, i 1) ] Le (i 1);
      ]
  in
  Alcotest.(check bool) "optimal" true (s = Flp.Optimal);
  Alcotest.(check (float 1e-9)) "value" 1.25 (Flp.value t);
  Alcotest.(check (array (float 1e-6))) "point" [| 1.; 0.; 1.; 0. |] (Flp.x t)

let infeasible () =
  let _, s =
    Flp.solve ~vars:1
      [ (0, i 1) ]
      [ row [ (0, i 1) ] Le (i 1); row [ (0, i 1) ] Ge (i 2) ]
  in
  Alcotest.(check bool) "infeasible" true (s = Flp.Infeasible)

let rel = QCheck2.Gen.oneof_list [ Lp.Le; Lp.Le; Lp.Ge; Lp.Eq ]

let lp n =
  QCheck2.Gen.(
    pair
      (list_size (int_range 1 6)
         (triple
            (array_size (pure n) (int_range (-4) 4))
            rel (int_range (-3) 9)))
      (array_size (pure n) (int_range (-5) 5)))

let rows_of n gen =
  List.map
    (fun (a, r, b) -> row (List.init n (fun j -> (j, i a.(j)))) r (i b))
    gen
  @ List.init n (fun j -> row [ (j, i 1) ] Le (i 3))

let random () =
  prop ~count:400 "matches exact" (lp 4) (fun (gen, obj) ->
      let rows = rows_of 4 gen
      and obj = List.init 4 (fun j -> (j, i obj.(j))) in
      let t, s = Flp.solve ~vars:4 obj rows in
      match (exact ~vars:4 obj rows, s) with
      | Some v, Flp.Optimal -> close v (Flp.value t) && Flp.violation t < 1e-7
      | None, Flp.Infeasible -> true
      | _ -> false)

let degenerate () =
  let gen =
    QCheck2.Gen.(
      pair
        (list_size (int_range 1 4) (array_size (pure 4) (int_range (-3) 3)))
        (array_size (pure 4) (int_range (-4) 4)))
  in
  prop ~count:200 "duplicated equalities" gen (fun (eqs, obj) ->
      let eq a =
        row
          (List.init 4 (fun j -> (j, i a.(j))))
          Eq
          (i (Array.fold_left ( + ) 0 a))
      in
      let rows =
        List.map eq eqs @ List.map eq eqs
        @ List.init 4 (fun j -> row [ (j, i 1) ] Le (i 2))
        @ [ row [ (0, i 1); (1, i 1) ] Le (i 2) ]
      in
      let obj = List.init 4 (fun j -> (j, i obj.(j))) in
      let t, s = Flp.solve ~vars:4 obj rows in
      match exact ~vars:4 obj rows with
      | Some v -> s = Flp.Optimal && close v (Flp.value t)
      | None -> false)

let cuts n =
  QCheck2.Gen.(
    list_size (int_range 1 4)
      (triple
         (array_size (pure n) (int_range (-4) 4))
         (oneof_list [ Lp.Le; Lp.Ge ])
         (int_range (-2) 8)))

let warm () =
  prop ~count:400 "warm rows match cold"
    (QCheck2.Gen.pair (lp 5) (cuts 5))
    (fun ((gen, obj), extra) ->
      let rows = rows_of 5 gen
      and obj = List.init 5 (fun j -> (j, i obj.(j))) in
      let more =
        List.map
          (fun (a, r, b) -> row (List.init 5 (fun j -> (j, i a.(j)))) r (i b))
          extra
      in
      match Flp.solve ~vars:5 obj rows with
      | t, Flp.Optimal -> (
          let t = Flp.thaw (Flp.freeze t) in
          let s = Flp.add t more in
          match (exact ~vars:5 obj (rows @ more), s) with
          | Some v, Flp.Optimal ->
              close v (Flp.value t) && Flp.violation t < 1e-7
          | None, Flp.Infeasible -> true
          | _ -> false)
      | _ -> QCheck2.assume_fail ())

let chain () =
  let obj = [ (0, i 1); (1, i 1); (2, i 1) ] in
  let base = [ row [ (0, i 1); (1, i 1); (2, i 1) ] Le (i 10) ] in
  let t, _ = Flp.solve ~vars:3 obj base in
  let rec go t k =
    if k > 0 then begin
      let cut = row [ (k mod 3, i 1) ] Le (q k 2) in
      Alcotest.(check bool) "optimal" true (Flp.add t [ cut ] = Flp.Optimal);
      go (Flp.thaw (Flp.freeze t)) (k - 1)
    end
    else t
  in
  let t = go t 6 in
  Alcotest.(check (float 1e-9)) "value" 3. (Flp.value t);
  Alcotest.(check int) "rows" 7 (Flp.rows t)

let stopped () =
  let obj = [ (0, i 1); (1, i 1) ] in
  let t, _ = Flp.solve ~vars:2 obj [ row [ (0, i 1); (1, i 1) ] Le (i 4) ] in
  match
    Flp.add ~limit:0 t [ row [ (0, i 1) ] Le (i 1); row [ (1, i 1) ] Le (i 1) ]
  with
  | Flp.Bound ->
      Alcotest.(check bool) "upper bound" true (Flp.value t >= 2. -. 1e-9)
  | Optimal | Infeasible | Failed -> Alcotest.fail "expected a stopped bound"

let tests =
  [
    Alcotest.test_case "wyndor" `Quick wyndor;
    Alcotest.test_case "beale" `Quick beale;
    Alcotest.test_case "infeasible" `Quick infeasible;
    random ();
    degenerate ();
    warm ();
    Alcotest.test_case "chain of cuts" `Quick chain;
    Alcotest.test_case "stopped dual" `Quick stopped;
  ]
