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
    Alcotest.test_case "arguments" `Quick rejects;
  ]
