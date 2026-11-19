open Cfr_solver

let prop ~count name gen f =
  Alcotest.test_case name `Quick (fun () ->
      QCheck2.Test.check_exn (QCheck2.Test.make ~count ~name gen f))

let q = Rat.of_ints
let i = Rat.of_int
let rat = Alcotest.testable Rat.pp Rat.equal
let row coeffs rel rhs = { Lp.coeffs; rel; rhs }

let optimum ?bland ?warm ~vars obj rows =
  match Lp.maximize ?bland ?warm ~vars obj rows with
  | Lp.Optimal { value; x; _ } -> (value, x)
  | Infeasible -> Alcotest.fail "infeasible"
  | Unbounded -> Alcotest.fail "unbounded"

let check_point name expected (value, x) v =
  Alcotest.check rat (name ^ " value") v value;
  Alcotest.(check (array rat)) (name ^ " point") expected x

let wyndor () =
  check_point "wyndor"
    [| i 2; i 6 |]
    (optimum ~vars:2
       [ (0, i 3); (1, i 5) ]
       [
         row [ (0, i 1) ] Le (i 4);
         row [ (1, i 2) ] Le (i 12);
         row [ (0, i 3); (1, i 2) ] Le (i 18);
       ])
    (i 36)

let phase_one () =
  check_point "diet"
    [| i 3; i 1 |]
    (optimum ~vars:2
       [ (0, i (-2)); (1, i (-3)) ]
       [
         row [ (0, i 1); (1, i 1) ] Ge (i 4);
         row [ (0, i 1); (1, i 3) ] Ge (i 6);
       ])
    (i (-9));
  check_point "equality"
    [| i 0; q 7 2 |]
    (optimum ~vars:2
       [ (0, i 1); (1, i 1) ]
       [
         row [ (0, i 3); (1, i 2) ] Eq (i 7);
         row [ (0, i 1); (1, i (-1)) ] Le (q 1 2);
       ])
    (q 7 2);
  check_point "redundant"
    [| i 1; i 0 |]
    (optimum ~vars:2
       [ (0, i 1) ]
       [
         row [ (0, i 1); (1, i 1) ] Eq (i 1);
         row [ (0, i 2); (1, i 2) ] Eq (i 2);
       ])
    (i 1);
  check_point "negative rhs"
    [| i 3 |]
    (optimum ~vars:1 [ (0, i 1) ] [ row [ (0, i (-1)) ] Ge (i (-3)) ])
    (i 3)

let beale bland warm () =
  let value, x =
    optimum ~bland ~warm ~vars:4
      [ (0, q 3 4); (1, i (-20)); (2, q 1 2); (3, i (-6)) ]
      [
        row [ (0, q 1 4); (1, i (-8)); (2, i (-1)); (3, i 9) ] Le (i 0);
        row [ (0, q 1 2); (1, i (-12)); (2, q (-1) 2); (3, i 3) ] Le (i 0);
        row [ (2, i 1) ] Le (i 1);
      ]
  in
  Alcotest.check rat "value" (q 5 4) value;
  Alcotest.(check (array rat)) "point" [| i 1; i 0; i 1; i 0 |] x

let failures () =
  let status = function
    | Lp.Optimal _ -> "optimal"
    | Infeasible -> "infeasible"
    | Unbounded -> "unbounded"
  in
  Alcotest.(check string)
    "infeasible" "infeasible"
    (status
       (Lp.maximize ~vars:2
          [ (0, i 1) ]
          [
            row [ (0, i 1); (1, i 1) ] Le (i 1);
            row [ (0, i 1); (1, i 1) ] Ge (i 2);
          ]));
  Alcotest.(check string)
    "unbounded" "unbounded"
    (status
       (Lp.maximize ~vars:2
          [ (0, i 1) ]
          [ row [ (0, i 1); (1, i (-1)) ] Le (i 1) ]))

let vertices rows obj =
  let lines =
    ((1, 0), i 0)
    :: ((0, 1), i 0)
    :: List.map (fun ((a, b), c) -> ((a, b), i c)) rows
  in
  let feasible x y =
    Rat.sign x >= 0
    && Rat.sign y >= 0
    && List.for_all
         (fun ((a, b), c) ->
           Rat.compare (Rat.add (Rat.mul (i a) x) (Rat.mul (i b) y)) (i c) <= 0)
         rows
  in
  let best = ref None in
  List.iter
    (fun ((a, b), c) ->
      List.iter
        (fun ((a', b'), c') ->
          let det = (a * b') - (b * a') in
          if det <> 0 then
            let d = i det in
            let x = Rat.div (Rat.sub (Rat.mul c (i b')) (Rat.mul c' (i b))) d
            and y = Rat.div (Rat.sub (Rat.mul (i a) c') (Rat.mul (i a') c)) d in
            if feasible x y then
              let v =
                Rat.add (Rat.mul (i (fst obj)) x) (Rat.mul (i (snd obj)) y)
              in
              match !best with
              | Some b when Rat.compare b v >= 0 -> ()
              | _ -> best := Some v)
        lines)
    lines;
  Option.get !best

let random_2d () =
  let gen =
    QCheck2.Gen.(
      pair
        (list_size (int_range 0 5)
           (pair (pair (int_range (-6) 6) (int_range (-6) 6)) (int_range 0 20)))
        (pair (int_range (-5) 5) (int_range (-5) 5)))
  in
  prop ~count:300 "matches vertex enumeration" gen (fun (rows, obj) ->
      let rows = ((1, 0), 10) :: ((0, 1), 10) :: rows in
      let solve warm =
        fst
          (optimum ~warm ~vars:2
             [ (0, i (fst obj)); (1, i (snd obj)) ]
             (List.map
                (fun ((a, b), c) -> row [ (0, i a); (1, i b) ] Le (i c))
                rows))
      in
      let value = solve true in
      Rat.equal value (solve false) && Rat.equal value (vertices rows obj))

let beale_bound () =
  match
    Lp.bound_float ~vars:4
      [ (0, q 3 4); (1, i (-20)); (2, q 1 2); (3, i (-6)) ]
      [
        row [ (0, q 1 4); (1, i (-8)); (2, i (-1)); (3, i 9) ] Le (i 0);
        row [ (0, q 1 2); (1, i (-12)); (2, q (-1) 2); (3, i 3) ] Le (i 0);
        row [ (2, i 1) ] Le (i 1);
      ]
  with
  | Some (v, x) ->
      Alcotest.(check (float 1e-9)) "value" 1.25 v;
      Alcotest.(check (array (float 1e-6))) "point" [| 1.; 0.; 1.; 0. |] x
  | None -> Alcotest.fail "no bound"

let degenerate_bound () =
  let gen =
    QCheck2.Gen.(
      pair
        (list_size (int_range 1 4) (array_size (pure 4) (int_range (-3) 3)))
        (array_size (pure 4) (int_range (-4) 4)))
  in
  prop ~count:200 "bound matches exact on degenerate LPs" gen (fun (eqs, obj) ->
      let center = [| 1; 1; 1; 1 |] in
      let eq a =
        let rhs =
          Array.fold_left ( + ) 0 (Array.mapi (fun j c -> c * center.(j)) a)
        in
        row (List.init 4 (fun j -> (j, i a.(j)))) Eq (i rhs)
      in
      let rows =
        List.map eq eqs @ List.map eq eqs
        @ List.init 4 (fun j -> row [ (j, i 1) ] Le (i 2))
        @ [ row [ (0, i 1); (1, i 1) ] Le (i 2) ]
      in
      let obj = List.init 4 (fun j -> (j, i obj.(j))) in
      match (Lp.maximize ~vars:4 obj rows, Lp.bound_float ~vars:4 obj rows) with
      | Lp.Optimal { value; _ }, Some (v, _) ->
          Float.abs (v -. Rat.to_float value) < 1e-7
      | _ -> false)

let random_lp =
  QCheck2.Gen.(
    int_range 1 4 >>= fun vars ->
    let coeffs = array_size (pure vars) (int_range (-3) 3) in
    let rel = oneof_list [ Lp.Le; Lp.Ge; Lp.Eq ] in
    quad (pure vars)
      (list_size (int_range 1 5) (triple coeffs rel (int_range (-6) 6)))
      coeffs int)

let resumed () =
  prop ~count:400 "resume from any basis matches dense" random_lp
    (fun (vars, rows, obj, seed) ->
      let rows =
        List.map
          (fun (a, rel, b) ->
            row (List.init vars (fun j -> (j, i a.(j)))) rel (i b))
          rows
        @ List.init vars (fun j -> row [ (j, i 1) ] Le (i 4))
      in
      let obj = List.init vars (fun j -> (j, i obj.(j))) in
      let m = List.length rows in
      let rel (r : Lp.row) =
        match r.rel with
        | (Le | Ge) when Rat.sign r.rhs < 0 -> if r.rel = Le then Lp.Ge else Le
        | rel -> rel
      in
      let n =
        vars
        + List.length (List.filter (fun r -> rel r <> Eq) rows)
        + List.length (List.filter (fun r -> rel r <> Le) rows)
      in
      let st = Random.State.make [| seed |] in
      let perm = Array.init n Fun.id in
      for k = n - 1 downto 1 do
        let r = Random.State.int st (k + 1) in
        let t = perm.(k) in
        perm.(k) <- perm.(r);
        perm.(r) <- t
      done;
      let basis = Array.sub perm 0 m in
      match
        ( Lp.resume ~basis ~vars obj rows,
          Lp.maximize ~warm:false ~bland:true ~vars obj rows )
      with
      | Optimal a, Optimal b -> Rat.equal a.value b.value
      | Infeasible, Infeasible | Unbounded, Unbounded -> true
      | _ -> false)

let dual_restart () =
  let rows =
    [
      row [ (0, i 1) ] Le (i 4);
      row [ (1, i 2) ] Le (i 12);
      row [ (0, i 3); (1, i 2) ] Le (i 18);
    ]
  in
  List.iter
    (fun basis ->
      match Lp.resume ~basis ~vars:2 [ (0, i 3); (1, i 5) ] rows with
      | Optimal { value; x; pivots } ->
          Alcotest.check rat "value" (i 36) value;
          Alcotest.(check (array rat)) "point" [| i 2; i 6 |] x;
          Alcotest.(check int) "pivots" 1 pivots
      | _ -> Alcotest.fail "not optimal")
    [ [| 0; 1; 3 |]; [| 0; 1; 4 |] ]

let transport () =
  let n = 24 in
  let v a b = (a * n) + b in
  let cost a b = ((a * 7) + (b * 13) + (a * b * 5)) mod 17 in
  let rows =
    List.init n (fun a ->
        row (List.init n (fun b -> (v a b, i 1))) Eq (i (a + 1)))
    @ List.init n (fun b ->
        row (List.init n (fun a -> (v a b, i 1))) Le (i (n + 1 - b)))
  in
  let obj =
    List.concat
      (List.init n (fun a -> List.init n (fun b -> (v a b, i (-cost a b)))))
  in
  let dense = fst (optimum ~warm:false ~vars:(n * n) obj rows) in
  let basis =
    Array.init (2 * n) (fun r -> (n * n) + if r < n then n + r else r - n)
  in
  match Lp.resume ~basis ~vars:(n * n) obj rows with
  | Optimal { value; pivots; _ } ->
      Alcotest.check rat "value" dense value;
      Alcotest.(check bool)
        (Printf.sprintf "%d pivots" pivots)
        true (pivots > 32)
  | _ -> Alcotest.fail "not optimal"

let tests =
  [
    Alcotest.test_case "wyndor" `Quick wyndor;
    Alcotest.test_case "phase one" `Quick phase_one;
    Alcotest.test_case "beale, bland" `Quick (beale true false);
    Alcotest.test_case "beale, dantzig" `Quick (beale false false);
    Alcotest.test_case "beale, warm start" `Quick (beale false true);
    Alcotest.test_case "infeasible and unbounded" `Quick failures;
    random_2d ();
    Alcotest.test_case "bound on beale" `Quick beale_bound;
    degenerate_bound ();
    resumed ();
    Alcotest.test_case "resume from a bad basis" `Quick dual_restart;
    Alcotest.test_case "transport past the eta limit" `Quick transport;
  ]
