type t = { name : string; dim : int; encode : string -> float array }

let fail key = invalid_arg ("Features: unexpected infoset " ^ key)

let moves key x ~offset ~slots ~symbols s =
  if String.length s > slots then fail key;
  String.iteri
    (fun i c ->
      match String.index_opt symbols c with
      | Some j -> x.(offset + (i * String.length symbols) + j) <- 1.
      | None -> fail key)
    s

let kuhn =
  let dim = 3 + (2 * 2) in
  let encode key =
    let x = Array.make dim 0. in
    if key = "" then fail key;
    (match String.index_opt "JQK" key.[0] with
    | Some r -> x.(r) <- 1.
    | None -> fail key);
    moves key x ~offset:3 ~slots:2 ~symbols:"pb"
      (String.sub key 1 (String.length key - 1));
    x
  in
  { name = "kuhn"; dim; encode }

let leduc (d : Leduc.deck) =
  let ranks = d.ranks in
  let slots = 4 in
  let dim = (2 * ranks) + (2 * slots * 2) in
  let rank key c =
    match
      List.find_opt
        (fun r -> Leduc.rank_name d r = String.make 1 c)
        (List.init ranks Fun.id)
    with
    | Some r -> r
    | None -> fail key
  in
  let encode key =
    let x = Array.make dim 0. in
    let cards, history =
      match String.index_opt key ':' with
      | Some i ->
          ( String.sub key 0 i,
            String.sub key (i + 1) (String.length key - i - 1) )
      | None -> fail key
    in
    let first, second =
      match String.split_on_char '/' history with
      | [ a ] -> (a, None)
      | [ a; b ] -> (a, Some b)
      | _ -> fail key
    in
    (match (String.length cards, second) with
    | 1, None -> x.(rank key cards.[0]) <- 1.
    | 2, Some _ ->
        x.(rank key cards.[0]) <- 1.;
        x.(ranks + rank key cards.[1]) <- 1.
    | _ -> fail key);
    let round offset s = moves key x ~offset ~slots ~symbols:"cr" s in
    round (2 * ranks) first;
    Option.iter (round ((2 * ranks) + (slots * 2))) second;
    x
  in
  { name = "leduc"; dim; encode }

let one_hot keys =
  let index = Hashtbl.create (Array.length keys) in
  Array.iteri (fun i k -> Hashtbl.replace index k i) keys;
  let dim = Array.length keys in
  let encode key =
    match Hashtbl.find_opt index key with
    | Some i -> Array.init dim (fun j -> if i = j then 1. else 0.)
    | None -> fail key
  in
  { name = "one-hot"; dim; encode }
