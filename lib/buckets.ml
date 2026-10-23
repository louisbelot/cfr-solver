type feature = Ehs | Ehs2 | Hist of int
type t = { name : string; labels : int array }

let feature_name = function Ehs -> "ehs" | Ehs2 -> "ehs2" | Hist _ -> "emd"

let round1 ?(seed = 1) (d : Leduc.deck) feature k =
  let ranks = Array.init d.ranks Fun.id in
  let labels =
    match feature with
    | Ehs -> Cluster.optimal_1d (Array.map (Strength.ehs d) ranks) k
    | Ehs2 -> Cluster.optimal_1d (Array.map (Strength.ehs2 d) ranks) k
    | Hist bins ->
        Cluster.kmedians (Rng.make seed)
          (Array.map (Strength.histogram ~bins d) ranks)
          k
  in
  { name = feature_name feature ^ string_of_int k; labels }

let round2 (d : Leduc.deck) k =
  let pairs =
    List.init d.ranks (fun c ->
        List.map (fun (p, b) -> (c, b, p)) (Strength.boards d c))
    |> List.concat |> Array.of_list
  in
  let hs = Array.map (fun (c, b, _) -> Strength.hs d c b) pairs
  and weights = Array.map (fun (_, _, p) -> p) pairs in
  let found = Cluster.optimal_1d ~weights hs k in
  let labels = Array.make (d.ranks * d.ranks) (-1) in
  Array.iteri (fun i (c, b, _) -> labels.((c * d.ranks) + b) <- found.(i)) pairs;
  { name = "hs" ^ string_of_int k; labels }

let abstraction ?round1 ?round2 ?(recall = true) (d : Leduc.deck) =
  let first c =
    match round1 with
    | None -> Leduc.rank_name d c
    | Some r -> "b" ^ string_of_int r.labels.(c)
  in
  let second =
    match round2 with
    | None -> fun c b -> first c ^ Leduc.rank_name d b
    | Some r ->
        fun c b ->
          (if recall then first c else "")
          ^ "."
          ^ string_of_int r.labels.((c * d.ranks) + b)
  in
  let part = function None -> "exact" | Some r -> r.name in
  {
    Leduc.name =
      (part round1 ^ "/" ^ part round2 ^ if recall then "" else " imperfect");
    round1 = first;
    round2 = second;
  }
