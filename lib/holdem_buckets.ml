type feature = Ehs2 | Hist of int | Emd | Potential of int

let feature_name = function
  | Ehs2 -> "ehs2"
  | Hist b -> Printf.sprintf "hist%d" b
  | Emd -> "emd"
  | Potential k -> Printf.sprintf "pot%d" k

let feature_of_string s =
  let n = String.length s in
  let count prefix =
    let p = String.length prefix in
    match int_of_string_opt (String.sub s p (n - p)) with
    | Some k when k > 0 -> k
    | _ -> invalid_arg ("Holdem_buckets: bad feature " ^ s)
  in
  match s with
  | "ehs2" -> Ehs2
  | "emd" -> Emd
  | _ when String.starts_with ~prefix:"hist" s -> Hist (count "hist")
  | _ when String.starts_with ~prefix:"pot" s -> Potential (count "pot")
  | _ -> invalid_arg ("Holdem_buckets: bad feature " ^ s)

let compact labels =
  let rank = Hashtbl.create 64 in
  List.iteri
    (fun r l -> Hashtbl.replace rank l r)
    (List.sort_uniq compare (Array.to_list labels));
  Array.map (Hashtbl.find rank) labels

let distinct points =
  let index = Hashtbl.create 256 and uniq = ref [] and count = ref 0 in
  let id =
    Array.map
      (fun x ->
        match Hashtbl.find_opt index x with
        | Some j -> j
        | None ->
            Hashtbl.add index x !count;
            uniq := x :: !uniq;
            incr count;
            !count - 1)
      points
  in
  let uniq = Array.of_list (List.rev !uniq) in
  let w = Array.make (Array.length uniq) 0. in
  Array.iter (fun j -> w.(j) <- w.(j) +. 1.) id;
  (uniq, w, id)

let clustered fit points k =
  let uniq, weights, id = distinct points in
  let labels = fit weights uniq k in
  compact (Array.map (fun j -> labels.(j)) id)

let labels ?(seed = 1) ?(restarts = 4) feature k board hands =
  let pick v = Array.map (fun c -> v.(Range.index c)) hands in
  let rng = Rng.make seed in
  match (Array.length board, feature) with
  | 5, _ -> Cluster.optimal_1d (pick (Equity.hs board)) k
  | 4, Ehs2 -> Cluster.optimal_1d (pick (Equity.ehs2 board)) k
  | 4, Hist bins ->
      clustered
        (fun weights -> Cluster.kmedians ~weights ~restarts rng)
        (pick (Equity.histogram ~bins board))
        k
  | 4, Emd ->
      clustered
        (fun weights -> Cluster.quantile_kmedians ~weights ~restarts rng)
        (pick (Equity.rivers board))
        k
  | 4, Potential q ->
      clustered
        (fun weights -> Cluster.quantile_kmedians ~weights ~restarts rng)
        (pick (Equity.rivers ~quantize:q board))
        k
  | _ -> invalid_arg "Holdem_buckets.labels: turn or river board"
