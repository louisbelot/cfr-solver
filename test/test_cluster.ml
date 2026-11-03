open Cfr_solver

let rec labelings n k =
  if n = 0 then [ [] ]
  else
    List.concat_map
      (fun rest -> List.init k (fun l -> l :: rest))
      (labelings (n - 1) k)

let brute ?weights xs k =
  List.fold_left
    (fun best l -> Float.min best (Cluster.sse ?weights xs (Array.of_list l)))
    infinity
    (labelings (Array.length xs) k)

let points =
  QCheck2.Gen.(
    let* n = int_range 1 7 and* k = int_range 1 3 in
    let* xs =
      array_size (return n) (map (fun i -> float_of_int i /. 4.) (int_bound 8))
    and* ws = array_size (return n) (float_range 0.1 3.) in
    return (xs, ws, k))

let optimal (xs, weights, k) =
  let labels = Cluster.optimal_1d ~weights xs k in
  Float.abs (Cluster.sse ~weights xs labels -. brute ~weights xs k) < 1e-9

let sorted (xs, weights, k) =
  let labels = Cluster.optimal_1d ~weights xs k in
  Array.for_all
    (fun i ->
      Array.for_all
        (fun j -> not (xs.(i) < xs.(j) && labels.(i) > labels.(j)))
        (Array.init (Array.length xs) Fun.id))
    (Array.init (Array.length xs) Fun.id)

let optimal_property () =
  QCheck2.Test.check_exn
    (QCheck2.Test.make ~count:300 ~name:"optimal" points optimal);
  QCheck2.Test.check_exn
    (QCheck2.Test.make ~count:300 ~name:"sorted" points sorted)

let by_hand () =
  let xs = [| 0.; 0.1; 0.9; 1.; 0.5 |] in
  Alcotest.(check (array int))
    "three" [| 0; 0; 2; 2; 1 |] (Cluster.optimal_1d xs 3);
  Alcotest.(check (array int))
    "ties together" [| 0; 1; 1; 0 |]
    (Cluster.optimal_1d [| 0.2; 0.7; 0.7; 0.2 |] 3);
  Alcotest.(check (array int)) "empty" [||] (Cluster.optimal_1d [||] 2)

let grid bins xs =
  let h = Array.make bins 0. in
  Array.iter
    (fun i -> h.(i) <- h.(i) +. (1. /. float_of_int (Array.length xs)))
    xs;
  h

let samples =
  QCheck2.Gen.(
    let* n = int_range 1 20 in
    pair
      (array_size (return n) (int_bound 15))
      (array_size (return n) (int_bound 15)))

let emd_sorted (a, b) =
  let a = Array.copy a and b = Array.copy b in
  Array.sort compare a;
  Array.sort compare b;
  let n = Array.length a in
  let direct = ref 0. in
  for i = 0 to n - 1 do
    direct := !direct +. float_of_int (abs (a.(i) - b.(i)))
  done;
  let direct = !direct /. float_of_int n /. 16. in
  Float.abs (Cluster.emd (grid 16 a) (grid 16 b) -. direct) < 1e-12

let emd_property () =
  QCheck2.Test.check_exn
    (QCheck2.Test.make ~count:300 ~name:"emd" samples emd_sorted)

let weighted =
  QCheck2.Gen.(
    array_size (int_range 1 9)
      (pair (float_bound_inclusive 1.) (float_range 0.1 2.)))

let median_minimizes pairs =
  let loss m =
    Array.fold_left
      (fun acc (x, w) -> acc +. (w *. Float.abs (x -. m)))
      0. pairs
  in
  let m = Cluster.median pairs in
  Array.for_all (fun (y, _) -> loss m <= loss y +. 1e-12) pairs

let median_property () =
  QCheck2.Test.check_exn
    (QCheck2.Test.make ~count:300 ~name:"median" weighted median_minimizes)

let blobs rng =
  Array.init 30 (fun i ->
      let h = Array.make 20 0.01 in
      let centre = [| 2; 10; 17 |].(i mod 3) in
      h.(centre + Rng.int rng 2) <- 1.;
      let total = Array.fold_left ( +. ) 0. h in
      Array.map (fun x -> x /. total) h)

let recovers () =
  let rng = Rng.make 4 in
  let hists = blobs rng in
  let labels = Cluster.kmedians rng hists 3 in
  Array.iteri
    (fun i l -> Alcotest.(check int) (string_of_int i) (i mod 3) l)
    labels

let fixed_point () =
  let rng = Rng.make 9 in
  let hists =
    Array.init 40 (fun _ ->
        let h = Array.init 12 (fun _ -> float_of_int (Rng.int rng 100)) in
        let total = Array.fold_left ( +. ) 0. h in
        Array.map (fun x -> x /. total) h)
  in
  let labels = Cluster.kmedians rng hists 5 in
  let centers = Cluster.centers hists labels in
  Array.iteri
    (fun i h ->
      let own = Cluster.emd h centers.(labels.(i)) in
      Array.iter
        (fun c ->
          if Array.length c > 0 && Cluster.emd h c < own -. 1e-12 then
            Alcotest.failf "point %d has a nearer center" i)
        centers)
    hists;
  let one = Cluster.emd_cost hists (Array.make 40 0) in
  if not (Cluster.emd_cost hists labels < one) then
    Alcotest.fail "no better than one cluster"

let rec permutations = function
  | [] -> [ [] ]
  | l ->
      List.concat_map
        (fun x ->
          List.map (fun p -> x :: p) (permutations (List.filter (( <> ) x) l)))
        l

let assignment (a, b) =
  let a = Array.map float_of_int a and b = Array.map float_of_int b in
  let n = Array.length a in
  let best =
    List.fold_left
      (fun best p ->
        let c = ref 0. in
        List.iteri (fun i j -> c := !c +. Float.abs (a.(i) -. b.(j))) p;
        Float.min best (!c /. float_of_int n))
      infinity
      (permutations (List.init n Fun.id))
  in
  Float.abs (Cluster.wasserstein a b -. best) < 1e-12

let on_grid (a, b) =
  let scale = Array.map (fun i -> float_of_int i /. 16.) in
  Float.abs
    (Cluster.wasserstein (scale a) (scale b)
    -. Cluster.emd (grid 16 a) (grid 16 b))
  < 1e-12

let wasserstein_property () =
  let small =
    QCheck2.Gen.(
      let* n = int_range 1 6 in
      pair
        (array_size (return n) (int_bound 9))
        (array_size (return n) (int_bound 9)))
  in
  QCheck2.Test.check_exn
    (QCheck2.Test.make ~count:200 ~name:"assignment" small assignment);
  QCheck2.Test.check_exn
    (QCheck2.Test.make ~count:300 ~name:"grid" samples on_grid)

let quantiles rng n =
  Array.init n (fun _ ->
      let shift = float_of_int (Rng.int rng 3) in
      let q =
        Array.init 15 (fun _ ->
            shift +. (float_of_int (Rng.int rng 100) /. 50.))
      in
      Array.sort compare q;
      q)

let quantile_centers w qs labels =
  let k = Array.fold_left max (-1) labels + 1 in
  Array.init k (fun l ->
      let members =
        List.filter
          (fun i -> labels.(i) = l)
          (List.init (Array.length qs) Fun.id)
      in
      Array.init 15 (fun b ->
          Cluster.median
            (Array.of_list (List.map (fun i -> (qs.(i).(b), w.(i))) members))))

let quantile_fixed_point () =
  let rng = Rng.make 3 in
  let qs = quantiles rng 50 in
  let w = Array.init 50 (fun i -> float_of_int (1 + (i mod 4))) in
  let labels = Cluster.quantile_kmedians ~weights:w rng qs 4 in
  let centers = quantile_centers w qs labels in
  let cost labels centers =
    let c = ref 0. in
    Array.iteri
      (fun i q ->
        c := !c +. (w.(i) *. Cluster.wasserstein q centers.(labels.(i))))
      qs;
    !c
  in
  Array.iteri
    (fun i q ->
      let own = Cluster.wasserstein q centers.(labels.(i)) in
      Array.iter
        (fun c ->
          if Cluster.wasserstein q c < own -. 1e-12 then
            Alcotest.failf "point %d has a nearer center" i)
        centers)
    qs;
  let means = Array.map (fun c -> Array.fold_left ( +. ) 0. c) centers in
  Array.iteri
    (fun l m -> if l > 0 && m < means.(l - 1) then Alcotest.fail "order")
    means;
  let one = Array.make 50 0 in
  if not (cost labels centers < cost one (quantile_centers w qs one)) then
    Alcotest.fail "no better than one cluster"

let quantile_shifts () =
  let rng = Rng.make 11 in
  let qs =
    Array.init 30 (fun i ->
        Array.init 8 (fun b ->
            float_of_int (i mod 3 * 10)
            +. (float_of_int b /. 8.)
            +. (float_of_int (Rng.int rng 10) /. 100.)))
  in
  Array.iter (Array.sort compare) qs;
  let labels = Cluster.quantile_kmedians rng qs 3 in
  Array.iteri
    (fun i l -> Alcotest.(check int) (string_of_int i) (i mod 3) l)
    labels

let rejects () =
  Alcotest.check_raises "weights"
    (Invalid_argument "Cluster: weights must be positive") (fun () ->
      ignore (Cluster.optimal_1d ~weights:[| 1.; 0. |] [| 0.; 1. |] 1));
  Alcotest.check_raises "k"
    (Invalid_argument "Cluster.optimal_1d: k must be positive") (fun () ->
      ignore (Cluster.optimal_1d [| 0. |] 0))

let tests =
  [
    Alcotest.test_case "1d k-means is optimal" `Quick optimal_property;
    Alcotest.test_case "1d k-means by hand" `Quick by_hand;
    Alcotest.test_case "emd of sorted samples" `Quick emd_property;
    Alcotest.test_case "weighted median" `Quick median_property;
    Alcotest.test_case "k-medians finds blobs" `Quick recovers;
    Alcotest.test_case "k-medians fixed point" `Quick fixed_point;
    Alcotest.test_case "wasserstein" `Quick wasserstein_property;
    Alcotest.test_case "quantile k-medians fixed point" `Quick
      quantile_fixed_point;
    Alcotest.test_case "quantile k-medians finds shifts" `Quick quantile_shifts;
    Alcotest.test_case "arguments" `Quick rejects;
  ]
