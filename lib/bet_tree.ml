type size = Pot of float | Chips of float | All_in
type street = { bets : size list; raises : size list; cap : int }
type config = { pot : float; stack : float; streets : street list }
type action = Fold | Check | Call | Bet of float

type node =
  | Decision of {
      player : int;
      pot : float;
      committed : float;
      to_call : float;
      actions : action array;
      children : node array;
    }
  | Chance of node
  | Folded of { folder : int; win : float }
  | Showdown of { win : float }

let limit ?(cap = 4) ~pot ~bet streets =
  let s = { bets = [ Chips bet ]; raises = [ Chips bet ]; cap } in
  { pot; stack = infinity; streets = List.init streets (fun _ -> s) }

let action_to_string = function
  | Fold -> "f"
  | Check -> "x"
  | Call -> "c"
  | Bet x -> Printf.sprintf "b%g" x

let fraction ~pot ~committed ~to_call = function
  | Bet x -> (x -. committed -. to_call) /. (pot +. to_call)
  | Fold | Check | Call -> invalid_arg "Bet_tree.fraction"

let check cfg =
  let positive = function
    | Pot f -> f > 0.
    | Chips x -> x > 0.
    | All_in -> true
  in
  if not (cfg.pot > 0. && cfg.stack >= 0.) then
    invalid_arg "Bet_tree.build: pot and stack";
  if cfg.streets = [] then invalid_arg "Bet_tree.build: no streets";
  List.iter
    (fun s ->
      if s.cap < 0 || not (List.for_all positive (s.bets @ s.raises)) then
        invalid_arg "Bet_tree.build: sizes")
    cfg.streets

let build cfg =
  check cfg;
  let rec street streets pot =
    match streets with
    | [] -> Showdown { win = pot /. 2. }
    | s :: rest ->
        let left = cfg.stack -. ((pot -. cfg.pot) /. 2.) in
        if left <= 0. then next rest pot
        else act s rest pot left [| 0.; 0. |] 0 0
  and next rest pot =
    match rest with
    | [] -> Showdown { win = pot /. 2. }
    | _ -> Chance (street rest pot)
  and act s rest pot left c p n =
    let q = 1 - p in
    let total = pot +. c.(0) +. c.(1) and to_call = c.(q) -. c.(p) in
    let sizes, base, scale =
      if to_call = 0. then (s.bets, c.(p), total)
      else (s.raises, c.(q), total +. to_call)
    in
    let amount = function
      | Pot f -> base +. (f *. scale)
      | Chips x -> base +. x
      | All_in -> left
    in
    let bets =
      if n >= s.cap then []
      else
        List.map (fun z -> Float.min (amount z) left) sizes
        |> List.filter (fun x -> x > c.(q))
        |> List.sort_uniq compare
    in
    let passive = if to_call = 0. then [ Check ] else [ Fold; Call ] in
    let actions = Array.of_list (passive @ List.map (fun x -> Bet x) bets) in
    let child = function
      | Fold -> Folded { folder = p; win = (pot /. 2.) +. c.(p) }
      | Check when p = 1 -> next rest pot
      | Check -> act s rest pot left c q n
      | Call -> next rest (pot +. (2. *. c.(q)))
      | Bet x ->
          let c = Array.copy c in
          c.(p) <- x;
          act s rest pot left c q (n + 1)
    in
    Decision
      {
        player = p;
        pot = total;
        committed = c.(p);
        to_call;
        actions;
        children = Array.map child actions;
      }
  in
  street cfg.streets cfg.pot

let rec fold ~decision ~chance ~leaf = function
  | Decision { children; _ } as d ->
      decision d (Array.map (fold ~decision ~chance ~leaf) children)
  | Chance child -> chance (fold ~decision ~chance ~leaf child)
  | (Folded _ | Showdown _) as t -> leaf t

let count =
  fold
    ~decision:(fun _ cs -> Array.fold_left ( + ) 1 cs)
    ~chance:Fun.id
    ~leaf:(fun _ -> 0)
