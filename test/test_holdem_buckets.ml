open Cfr_solver

let board = Hand.parse "Qs Jh 8s 5c"
let hands = (Range.full ~board ()).hands
let features = Holdem_buckets.[ Ehs2; Hist 20; Emd; Potential 6 ]

let names () =
  List.iter
    (fun f ->
      let name = Holdem_buckets.feature_name f in
      Alcotest.(check string)
        name name
        (Holdem_buckets.feature_name (Holdem_buckets.feature_of_string name)))
    features;
  List.iter
    (fun s ->
      match Holdem_buckets.feature_of_string s with
      | exception Invalid_argument _ -> ()
      | _ -> Alcotest.failf "accepted %s" s)
    [ "hist"; "pot0"; "histx"; "emd2"; "" ]

let compact () =
  List.iter
    (fun f ->
      List.iter
        (fun k ->
          let l = Holdem_buckets.labels f k board hands in
          let used = List.sort_uniq compare (Array.to_list l) in
          Alcotest.(check (list int))
            (Printf.sprintf "%s %d" (Holdem_buckets.feature_name f) k)
            (List.init (min k (List.length used)) Fun.id)
            used)
        [ 1; 5; 40 ])
    features

let equal_rivers_share_a_bucket () =
  let q = Equity.rivers board in
  List.iter
    (fun f ->
      let l = Holdem_buckets.labels f 12 board hands in
      let seen = Hashtbl.create 256 in
      Array.iteri
        (fun i c ->
          let x = q.(Range.index c) in
          match Hashtbl.find_opt seen x with
          | Some b when b <> l.(i) ->
              Alcotest.failf "%s split %s"
                (Holdem_buckets.feature_name f)
                (Range.combo_to_string c)
          | Some _ -> ()
          | None -> Hashtbl.add seen x l.(i))
        hands)
    features

let one_bucket_per_distribution () =
  let q = Equity.rivers board in
  let distinct =
    List.length
      (List.sort_uniq compare
         (Array.to_list (Array.map (fun c -> q.(Range.index c)) hands)))
  in
  let l = Holdem_buckets.labels Emd 1000 board hands in
  Alcotest.(check int) "buckets" distinct (Array.fold_left max (-1) l + 1)

let w1_cost labels =
  let rivers = Equity.rivers board in
  let q = Array.map (fun c -> rivers.(Range.index c)) hands in
  let k = Array.fold_left max (-1) labels + 1 in
  let total = ref 0. in
  for b = 0 to k - 1 do
    let members =
      List.filter (fun i -> labels.(i) = b) (List.init (Array.length q) Fun.id)
    in
    let center =
      Array.init 46 (fun j ->
          Cluster.median
            (Array.of_list (List.map (fun i -> (q.(i).(j), 1.)) members)))
    in
    List.iter
      (fun i -> total := !total +. Cluster.wasserstein q.(i) center)
      members
  done;
  !total

let emd_beats_ehs2_in_w1 () =
  let emd = w1_cost (Holdem_buckets.labels Emd 8 board hands)
  and ehs2 = w1_cost (Holdem_buckets.labels Ehs2 8 board hands) in
  if not (emd < ehs2) then Alcotest.failf "emd %g, ehs2 %g" emd ehs2

let river_is_hs () =
  let river = Hand.parse "Qs Jh 8s 5c 2d" in
  let live = (Range.full ~board:river ()).hands in
  let hs = Equity.hs river in
  List.iter
    (fun f ->
      Alcotest.(check (array int))
        (Holdem_buckets.feature_name f)
        (Cluster.optimal_1d (Array.map (fun c -> hs.(Range.index c)) live) 7)
        (Holdem_buckets.labels f 7 river live))
    features

let tests =
  [
    Alcotest.test_case "feature names" `Quick names;
    Alcotest.test_case "labels are compact" `Quick compact;
    Alcotest.test_case "equal rivers share a bucket" `Quick
      equal_rivers_share_a_bucket;
    Alcotest.test_case "one bucket per distribution" `Quick
      one_bucket_per_distribution;
    Alcotest.test_case "emd beats ehs2 in w1" `Quick emd_beats_ehs2_in_w1;
    Alcotest.test_case "river buckets are hs" `Quick river_is_hs;
  ]
