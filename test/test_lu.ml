open Cfr_solver

let i = Rat.of_int

let columns a =
  let n = Array.length a in
  Array.init n (fun j ->
      List.filter_map
        (fun r -> if a.(r).(j) = 0 then None else Some (r, i a.(r).(j)))
        (List.init n Fun.id))

let det a =
  let n = Array.length a in
  let m = Array.map (Array.map i) a in
  let d = ref Rat.one in
  for k = 0 to n - 1 do
    match
      List.find_opt
        (fun r -> not (Rat.is_zero m.(r).(k)))
        (List.init (n - k) (( + ) k))
    with
    | None -> d := Rat.zero
    | Some p ->
        if p <> k then (
          let tmp = m.(p) in
          m.(p) <- m.(k);
          m.(k) <- tmp;
          d := Rat.neg !d);
        d := Rat.mul !d m.(k).(k);
        for r = k + 1 to n - 1 do
          let f = Rat.div m.(r).(k) m.(k).(k) in
          for c = k to n - 1 do
            m.(r).(c) <- Rat.sub m.(r).(c) (Rat.mul f m.(k).(c))
          done
        done
  done;
  !d

let apply a x =
  Array.map
    (fun row ->
      Array.fold_left Rat.add Rat.zero
        (Array.mapi (fun j v -> Rat.mul (i v) x.(j)) row))
    a

let transpose a =
  let n = Array.length a in
  Array.init n (fun r -> Array.init n (fun c -> a.(c).(r)))

let matrix =
  QCheck2.Gen.(
    int_range 1 7 >>= fun n ->
    array_size (pure n)
      (array_size (pure n)
         (oneof_weighted [ (3, pure 0); (2, int_range (-4) 4) ])))

let vec n = QCheck2.Gen.(array_size (pure n) (int_range (-9) 9))

let solves () =
  let gen =
    QCheck2.Gen.(
      matrix >>= fun a ->
      triple (pure a) (vec (Array.length a)) (vec (Array.length a)))
  in
  QCheck2.Test.check_exn
    (QCheck2.Test.make ~count:500 ~name:"lu solves" gen (fun (a, b, c) ->
         let b = Array.map i b and c = Array.map i c in
         match Lu.factor (Array.length a) (columns a) with
         | None -> Rat.is_zero (det a)
         | Some lu ->
             (not (Rat.is_zero (det a)))
             && Array.for_all2 Rat.equal (apply a (Lu.solve lu b)) b
             && Array.for_all2 Rat.equal
                  (apply (transpose a) (Lu.solve_t lu c))
                  c))

let duplicates () =
  let lu = Lu.factor 2 [| [ (0, i 1); (0, i 2); (1, i 1) ]; [ (1, i 3) ] |] in
  match lu with
  | None -> Alcotest.fail "singular"
  | Some lu ->
      Alcotest.(check (array (testable Rat.pp Rat.equal)))
        "x"
        [| i 1; i 1 |]
        (Lu.solve lu [| i 3; i 4 |])

let cancel () =
  Alcotest.(check bool)
    "cancelled entry" true
    (Option.is_none (Lu.factor 2 [| [ (0, i 1); (0, i (-1)) ]; [ (1, i 1) ] |]))

let tests =
  [
    Alcotest.test_case "random matrices" `Quick solves;
    Alcotest.test_case "repeated entries add" `Quick duplicates;
    Alcotest.test_case "entries that cancel" `Quick cancel;
  ]
