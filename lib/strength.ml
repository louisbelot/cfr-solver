let left (d : Leduc.deck) dealt r =
  d.suits - List.length (List.filter (( = ) r) dealt)

let score d b x = if x = b then d.Leduc.ranks + x else x

let hs (d : Leduc.deck) c b =
  if left d [ c ] b < 1 then invalid_arg "Strength.hs: board not possible";
  let total = ref 0. in
  for o = 0 to d.ranks - 1 do
    let n = float_of_int (left d [ c; b ] o) in
    let mine = score d b c and theirs = score d b o in
    total :=
      !total
      +. if mine > theirs then n else if mine = theirs then n /. 2. else 0.
  done;
  !total /. float_of_int (Leduc.cards d - 2)

let boards (d : Leduc.deck) c =
  List.init d.ranks Fun.id
  |> List.filter_map (fun b ->
      match left d [ c ] b with
      | 0 -> None
      | n -> Some (float_of_int n /. float_of_int (Leduc.cards d - 1), b))

let moment k d c =
  List.fold_left
    (fun acc (p, b) -> acc +. (p *. (hs d c b ** float_of_int k)))
    0. (boards d c)

let ehs = moment 1
let ehs2 = moment 2

let histogram ~bins d c =
  let h = Array.make bins 0. in
  List.iter
    (fun (p, b) ->
      let i = min (bins - 1) (Float.to_int (hs d c b *. float_of_int bins)) in
      h.(i) <- h.(i) +. p)
    (boards d c);
  h
