open Cfr_solver

let prop ~count name gen f =
  Alcotest.test_case name `Quick (fun () ->
      QCheck2.Test.check_exn (QCheck2.Test.make ~count ~name gen f))

let columns a =
  let n = Array.length a in
  Array.init n (fun j ->
      List.filter_map
        (fun r -> if a.(r).(j) = 0. then None else Some (r, a.(r).(j)))
        (List.init n Fun.id))

let apply a x =
  Array.map
    (fun row ->
      let s = ref 0. in
      Array.iteri (fun j v -> s := !s +. (v *. x.(j))) row;
      !s)
    a

let transpose a =
  let n = Array.length a in
  Array.init n (fun r -> Array.init n (fun c -> a.(c).(r)))

let close a b =
  Array.for_all2
    (fun x y -> Float.abs (x -. y) <= 1e-9 *. (1. +. Float.abs y))
    a b

let rank a =
  let n = Array.length a in
  let m = Array.map Array.copy a in
  let rank = ref 0 in
  for c = 0 to n - 1 do
    let p = ref (-1) in
    for r = !rank to n - 1 do
      if
        Float.abs m.(r).(c) > 1e-9
        && (!p < 0 || Float.abs m.(r).(c) > Float.abs m.(!p).(c))
      then p := r
    done;
    if !p >= 0 then (
      let t = m.(!p) in
      m.(!p) <- m.(!rank);
      m.(!rank) <- t;
      for r = 0 to n - 1 do
        if r <> !rank then
          let f = m.(r).(c) /. m.(!rank).(c) in
          Array.iteri (fun k v -> m.(r).(k) <- v -. (f *. m.(!rank).(k))) m.(r)
      done;
      incr rank)
  done;
  !rank

let matrix =
  QCheck2.Gen.(
    int_range 1 12 >>= fun n ->
    array_size (pure n)
      (array_size (pure n)
         (oneof_weighted
            [ (4, pure 0.); (2, map float_of_int (int_range (-5) 5)) ]))
    >>= fun a -> pair (pure a) (array_size (pure n) (float_range (-10.) 10.)))

let solves () =
  prop ~count:500 "solves nonsingular systems both ways" matrix (fun (a, b) ->
      let n = Array.length a in
      match Flu.factor n (columns a) with
      | None -> rank a < n
      | Some lu ->
          rank a = n
          && close (apply a (Flu.solve lu b)) b
          && close (apply (transpose a) (Flu.solve_t lu b)) b)

let tiny_pivot () =
  let a = [| [| 1e-13; 1. |]; [| 1.; 1. |] |] in
  match Flu.factor 2 (columns a) with
  | None -> Alcotest.fail "singular"
  | Some lu ->
      let x = Flu.solve lu [| 1.; 2. |] in
      Alcotest.(check (array (float 1e-12))) "x" [| 1.; 1. |] x

let structural () =
  let a = [| [| 1.; 2.; 0. |]; [| 3.; 4.; 0. |]; [| 0.; 5.; 0. |] |] in
  Alcotest.(check bool) "empty column" true (Flu.factor 3 (columns a) = None);
  let a = [| [| 1.; 1.; 0. |]; [| 2.; 2.; 0. |]; [| 0.; 0.; 1. |] |] in
  Alcotest.(check bool) "repeated column" true (Flu.factor 3 (columns a) = None)

let tests =
  [
    solves ();
    Alcotest.test_case "threshold skips a tiny pivot" `Quick tiny_pivot;
    Alcotest.test_case "singular" `Quick structural;
  ]
