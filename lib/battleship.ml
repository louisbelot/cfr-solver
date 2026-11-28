type ship = { length : int; value : float }

type config = {
  height : int;
  width : int;
  ships : ship list;
  shots : int;
  repeat : bool;
  gamma : float;
}

let baseline =
  {
    height = 2;
    width = 2;
    ships = [ { length = 1; value = 1. } ];
    shots = 3;
    repeat = false;
    gamma = 2.;
  }

let name c =
  Printf.sprintf "battleship-%dx%d-%s-%d" c.height c.width
    (String.concat "." (List.map (fun s -> string_of_int s.length) c.ships))
    c.shots

let cell r col = Printf.sprintf "%c%d" (Char.chr (97 + r)) (col + 1)

let placements c length =
  let bit r col = 1 lsl ((r * c.width) + col) in
  let run dir rows cols step =
    List.concat_map
      (fun r ->
        List.init cols (fun col ->
            let mask = ref 0 in
            for i = 0 to length - 1 do
              let r', c' = step r col i in
              mask := !mask lor bit r' c'
            done;
            (Printf.sprintf "%c:%s" dir (cell r col), !mask)))
      (List.init rows Fun.id)
  in
  let h =
    if length <= c.width then
      run 'h' c.height (c.width - length + 1) (fun r col i -> (r, col + i))
    else []
  and v =
    if length > 1 && length <= c.height then
      run 'v' (c.height - length + 1) c.width (fun r col i -> (r + i, col))
    else []
  in
  h @ v

type side = {
  fleet : (int * float) list;
  fired : int;
  shots : int;
  key : string;
}

type history = { a : side; b : side }

let side h p = if p = 0 then h.a else h.b
let with_side h p s = if p = 0 then { h with a = s } else { h with b = s }
let sunk fired (mask, _) = mask land fired = mask

let damage s fired =
  List.fold_left
    (fun d sh -> if sunk fired sh then d +. snd sh else d)
    0. s.fleet

let game c =
  let cells = c.height * c.width in
  if
    c.height < 1 || c.width < 1 || cells > 62 || c.ships = [] || c.shots < 1
    || List.exists (fun s -> s.length < 1) c.ships
    || ((not c.repeat) && c.shots > cells)
  then invalid_arg "Battleship.game: config";
  let ships = Array.of_list c.ships in
  let n = Array.length ships in
  let options = Array.map (fun s -> placements c s.length) ships in
  let rec feasible used k =
    k = n
    || List.exists
         (fun (_, m) -> m land used = 0 && feasible (used lor m) (k + 1))
         options.(k)
  in
  if not (feasible 0 0) then invalid_arg "Battleship.game: ships do not fit";
  let labels = Array.init cells (fun i -> cell (i / c.width) (i mod c.width)) in
  let place h p k (label, mask) =
    let s = side h p in
    with_side h p
      {
        s with
        fleet = (mask, ships.(k).value) :: s.fleet;
        key = s.key ^ "/" ^ label;
      }
  in
  let shoot h p i =
    let s = side h p and o = side h (1 - p) in
    let fired = s.fired lor (1 lsl i) in
    let outcome =
      match List.find_opt (fun (m, _) -> m land (1 lsl i) <> 0) o.fleet with
      | None -> 'W'
      | Some sh -> if sunk fired sh then 'S' else 'H'
    in
    let h =
      with_side h p
        {
          s with
          fired;
          shots = s.shots + 1;
          key = Printf.sprintf "%s/%s:%c" s.key labels.(i) outcome;
        }
    in
    with_side h (1 - p) { o with key = o.key ^ "/x" ^ labels.(i) }
  in
  let defeated s fired = List.for_all (sunk fired) s.fleet in
  (module struct
    type nonrec history = history

    let name = name c
    let players = 2
    let empty p = { fleet = []; fired = 0; shots = 0; key = string_of_int p }
    let root = { a = empty 0; b = empty 1 }

    let node h =
      let placed = List.length h.a.fleet + List.length h.b.fleet in
      if placed < 2 * n then
        let p = placed mod 2 and k = placed / 2 in
        let s = side h p in
        let used = List.fold_left (fun u (m, _) -> u lor m) 0 s.fleet in
        Multi.Decision
          {
            player = p;
            infoset = s.key;
            actions =
              List.filter_map
                (fun ((label, m) as o) ->
                  if m land used = 0 && feasible (used lor m) (k + 1) then
                    Some (label, place h p k o)
                  else None)
                options.(k);
          }
      else
        let total = h.a.shots + h.b.shots in
        if
          total = 2 * c.shots
          || defeated h.a h.b.fired || defeated h.b h.a.fired
        then
          let d0 = damage h.a h.b.fired and d1 = damage h.b h.a.fired in
          Terminal [| d1 -. (c.gamma *. d0); d0 -. (c.gamma *. d1) |]
        else
          let p = total mod 2 in
          let s = side h p in
          Decision
            {
              player = p;
              infoset = s.key;
              actions =
                List.filter_map
                  (fun i ->
                    if c.repeat || s.fired land (1 lsl i) = 0 then
                      Some (labels.(i), shoot h p i)
                    else None)
                  (List.init cells Fun.id);
            }
  end : Multi.S)
