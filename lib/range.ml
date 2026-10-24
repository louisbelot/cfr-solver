type t = { hands : (Hand.card * Hand.card) array; weights : float array }

let combos =
  Array.of_list
    (List.concat (List.init 52 (fun a -> List.init a (fun b -> (a, b)))))

let index (a, b) =
  let a, b = if a > b then (a, b) else (b, a) in
  (a * (a - 1) / 2) + b

let combo_to_string (a, b) = Hand.to_string a ^ Hand.to_string b
let size r = Array.length r.hands
let total r = Array.fold_left ( +. ) 0. r.weights
let blocks cards (a, b) = Array.exists (fun c -> c = a || c = b) cards

let of_weights ?(board = [||]) w =
  let keep =
    List.filter
      (fun i -> w.(i) > 0. && not (blocks board combos.(i)))
      (List.init 1326 Fun.id)
  in
  {
    hands = Array.of_list (List.map (fun i -> combos.(i)) keep);
    weights = Array.of_list (List.map (fun i -> w.(i)) keep);
  }

let full ?board () = of_weights ?board (Array.make 1326 1.)

let rank_of s c =
  match String.index_opt "23456789TJQKA" (Char.uppercase_ascii c) with
  | Some r -> r
  | None -> invalid_arg ("Range.of_string: " ^ s)

let class_combos hi lo kind =
  List.concat_map
    (fun s1 ->
      List.filter_map
        (fun s2 ->
          let a = Hand.card hi s1 and b = Hand.card lo s2 in
          let ok =
            match kind with
            | `Pair -> s1 > s2
            | `Suited -> s1 = s2
            | `Offsuit -> s1 <> s2
            | `Any -> true
          in
          if ok then Some (index (a, b)) else None)
        [ 0; 1; 2; 3 ])
    [ 0; 1; 2; 3 ]

let token s =
  let n = String.length s in
  let plus = n > 0 && s.[n - 1] = '+' in
  let body = if plus then String.sub s 0 (n - 1) else s in
  match String.length body with
  | 4 ->
      let a = Hand.of_string (String.sub body 0 2)
      and b = Hand.of_string (String.sub body 2 2) in
      if a = b || plus then invalid_arg ("Range.of_string: " ^ s);
      [ index (a, b) ]
  | 2 | 3 ->
      let hi = rank_of s body.[0] and lo = rank_of s body.[1] in
      let kind =
        match (String.length body, hi = lo) with
        | 2, true -> `Pair
        | 2, false -> `Any
        | 3, false when Char.lowercase_ascii body.[2] = 's' -> `Suited
        | 3, false when Char.lowercase_ascii body.[2] = 'o' -> `Offsuit
        | _ -> invalid_arg ("Range.of_string: " ^ s)
      in
      let hi, lo = if hi >= lo then (hi, lo) else (lo, hi) in
      let classes =
        if not plus then [ (hi, lo) ]
        else if kind = `Pair then
          List.init (13 - hi) (fun k -> (hi + k, hi + k))
        else List.init (hi - lo) (fun k -> (hi, lo + k))
      in
      List.concat_map (fun (h, l) -> class_combos h l kind) classes
  | _ -> invalid_arg ("Range.of_string: " ^ s)

let of_string ?board s =
  let w = Array.make 1326 0. in
  String.split_on_char ',' s |> List.map String.trim
  |> List.filter (( <> ) "")
  |> List.iter (fun item ->
      let body, weight =
        match String.split_on_char ':' item with
        | [ body ] -> (body, 1.)
        | [ body; x ] -> (
            match float_of_string_opt x with
            | Some x when x >= 0. && x <= 1. -> (body, x)
            | _ -> invalid_arg ("Range.of_string: " ^ item))
        | _ -> invalid_arg ("Range.of_string: " ^ item)
      in
      let ids =
        if String.lowercase_ascii body = "random" then List.init 1326 Fun.id
        else token body
      in
      List.iter (fun i -> w.(i) <- weight) ids);
  of_weights ?board w
