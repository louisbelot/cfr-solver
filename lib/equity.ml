let hs board =
  if Array.length board <> 5 then invalid_arg "Equity.hs: five board cards";
  let strength =
    Array.map
      (fun (a, b) ->
        if Range.blocks board (a, b) then -1
        else Hand.eval (Array.append [| a; b |] board))
      Range.combos
  in
  let order =
    Array.of_list
      (List.filter (fun i -> strength.(i) >= 0) (List.init 1326 Fun.id))
  in
  Array.sort (fun i j -> compare strength.(i) strength.(j)) order;
  let n = Array.length order in
  let opponents = float_of_int (n - (2 * 46) + 1) in
  let out = Array.make 1326 Float.nan in
  let below = Array.make 52 0 and tied = Array.make 52 0 in
  let less = ref 0 and x = ref 0 in
  while !x < n do
    let s = strength.(order.(!x)) in
    let y = ref !x in
    while !y < n && strength.(order.(!y)) = s do
      incr y
    done;
    Array.fill tied 0 52 0;
    for z = !x to !y - 1 do
      let a, b = Range.combos.(order.(z)) in
      tied.(a) <- tied.(a) + 1;
      tied.(b) <- tied.(b) + 1
    done;
    let eq = !y - !x in
    for z = !x to !y - 1 do
      let i = order.(z) in
      let a, b = Range.combos.(i) in
      let wins = !less - below.(a) - below.(b)
      and ties = eq - tied.(a) - tied.(b) + 1 in
      out.(i) <- (float_of_int wins +. (0.5 *. float_of_int ties)) /. opponents
    done;
    for z = !x to !y - 1 do
      let a, b = Range.combos.(order.(z)) in
      below.(a) <- below.(a) + 1;
      below.(b) <- below.(b) + 1
    done;
    less := !less + eq;
    x := !y
  done;
  out

let centroids v k =
  let live =
    List.filter (fun i -> not (Float.is_nan v.(i))) (List.init 1326 Fun.id)
  in
  let xs = Array.of_list (List.map (fun i -> v.(i)) live) in
  let labels = Cluster.optimal_1d xs k in
  let k = Array.fold_left max (-1) labels + 1 in
  let s = Array.make k 0. and n = Array.make k 0 in
  Array.iteri
    (fun j l ->
      s.(l) <- s.(l) +. xs.(j);
      n.(l) <- n.(l) + 1)
    labels;
  let out = Array.copy v in
  List.iteri
    (fun j i -> out.(i) <- s.(labels.(j)) /. float_of_int n.(labels.(j)))
    live;
  out

let rivers ?quantize board =
  if Array.length board <> 4 then invalid_arg "Equity.rivers: four board cards";
  let out = Array.make 1326 [] in
  for r = 51 downto 0 do
    if not (Array.mem r board) then begin
      let v = hs (Array.append board [| r |]) in
      let v = match quantize with Some k -> centroids v k | None -> v in
      Array.iteri
        (fun i (a, b) ->
          if a <> r && b <> r && not (Range.blocks board (a, b)) then
            out.(i) <- v.(i) :: out.(i))
        Range.combos
    end
  done;
  Array.map
    (fun l ->
      let q = Array.of_list l in
      Array.sort compare q;
      q)
    out

let ehs2 board =
  if Array.length board <> 4 then invalid_arg "Equity.ehs2: four board cards";
  Array.map
    (fun q ->
      if q = [||] then Float.nan
      else
        Array.fold_left (fun acc x -> acc +. (x *. x)) 0. q
        /. float_of_int (Array.length q))
    (rivers board)

let histogram ~bins board =
  Array.map
    (fun q ->
      if q = [||] then [||]
      else begin
        let h = Array.make bins 0. in
        Array.iter
          (fun x ->
            let b = min (bins - 1) (int_of_float (x *. float_of_int bins)) in
            h.(b) <- h.(b) +. (1. /. float_of_int (Array.length q)))
          q;
        h
      end)
    (rivers board)
