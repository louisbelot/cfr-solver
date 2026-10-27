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

let ehs2 board =
  if Array.length board <> 4 then invalid_arg "Equity.ehs2: four board cards";
  let sum = Array.make 1326 0. in
  for r = 0 to 51 do
    if not (Array.mem r board) then begin
      let v = hs (Array.append board [| r |]) in
      Array.iteri
        (fun i (a, b) ->
          if a <> r && b <> r && not (Range.blocks board (a, b)) then
            sum.(i) <- sum.(i) +. (v.(i) *. v.(i)))
        Range.combos
    end
  done;
  Array.mapi
    (fun i x ->
      if Range.blocks board Range.combos.(i) then Float.nan else x /. 46.)
    sum
