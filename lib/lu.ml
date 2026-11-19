type t = {
  n : int;
  prow : int array;
  pcol : int array;
  diag : Rat.t array;
  upper : (int * Rat.t) array array;
  lower : (int * Rat.t) array array;
}

let size (q : Rat.t) = Bigint.numbits q.num + Bigint.numbits q.den
let get tbl k = Option.value (Hashtbl.find_opt tbl k) ~default:Rat.zero

let update rows cols i j v =
  if Rat.is_zero v then (
    Hashtbl.remove rows.(i) j;
    Hashtbl.remove cols.(j) i)
  else (
    Hashtbl.replace rows.(i) j v;
    Hashtbl.replace cols.(j) i ())

exception Singular

let choose n rows cols col_done =
  let best = ref (-1, -1) and cost = ref max_int and bits = ref max_int in
  (try
     for j = 0 to n - 1 do
       if not col_done.(j) then begin
         let cc = Hashtbl.length cols.(j) in
         if cc = 0 then raise Singular;
         Hashtbl.iter
           (fun i () ->
             let c = (Hashtbl.length rows.(i) - 1) * (cc - 1) in
             if c <= !cost then
               let b = size (Hashtbl.find rows.(i) j) in
               if c < !cost || b < !bits then (
                 best := (i, j);
                 cost := c;
                 bits := b))
           cols.(j);
         if !cost = 0 && !bits <= 2 then raise Exit
       end
     done
   with Exit -> ());
  !best

let factor n columns =
  let rows = Array.init n (fun _ -> Hashtbl.create 8) in
  let cols = Array.init n (fun _ -> Hashtbl.create 8) in
  Array.iteri
    (fun j col ->
      List.iter
        (fun (i, v) -> update rows cols i j (Rat.add (get rows.(i) j) v))
        col)
    columns;
  let col_done = Array.make n false in
  let prow = Array.make n 0 and pcol = Array.make n 0 in
  let diag = Array.make n Rat.zero in
  let upper = Array.make n [||] and lower = Array.make n [||] in
  try
    for k = 0 to n - 1 do
      let i, j = choose n rows cols col_done in
      let p = Hashtbl.find rows.(i) j in
      let row =
        Hashtbl.fold
          (fun c v acc -> if c = j then acc else (c, v) :: acc)
          rows.(i) []
      in
      let others =
        Hashtbl.fold
          (fun r () acc -> if r = i then acc else r :: acc)
          cols.(j) []
      in
      let mults =
        List.map
          (fun r ->
            let l = Rat.div (Hashtbl.find rows.(r) j) p in
            Hashtbl.remove rows.(r) j;
            List.iter
              (fun (c, v) ->
                update rows cols r c (Rat.sub (get rows.(r) c) (Rat.mul l v)))
              row;
            (r, l))
          others
      in
      List.iter (fun (c, _) -> Hashtbl.remove cols.(c) i) row;
      Hashtbl.reset rows.(i);
      Hashtbl.reset cols.(j);
      col_done.(j) <- true;
      prow.(k) <- i;
      pcol.(k) <- j;
      diag.(k) <- p;
      upper.(k) <- Array.of_list row;
      lower.(k) <- Array.of_list mults
    done;
    Some { n; prow; pcol; diag; upper; lower }
  with Singular -> None

let solve t b =
  let w = Array.copy b in
  for k = 0 to t.n - 1 do
    let v = w.(t.prow.(k)) in
    if not (Rat.is_zero v) then
      Array.iter
        (fun (r, l) -> w.(r) <- Rat.sub w.(r) (Rat.mul l v))
        t.lower.(k)
  done;
  let x = Array.make t.n Rat.zero in
  for k = t.n - 1 downto 0 do
    let s =
      Array.fold_left
        (fun acc (c, u) -> Rat.sub acc (Rat.mul u x.(c)))
        w.(t.prow.(k))
        t.upper.(k)
    in
    x.(t.pcol.(k)) <- Rat.div s t.diag.(k)
  done;
  x

let solve_t t c =
  let acc = Array.make t.n Rat.zero and y = Array.make t.n Rat.zero in
  for k = 0 to t.n - 1 do
    let j = t.pcol.(k) in
    let z = Rat.div (Rat.sub c.(j) acc.(j)) t.diag.(k) in
    y.(t.prow.(k)) <- z;
    if not (Rat.is_zero z) then
      Array.iter
        (fun (c, u) -> acc.(c) <- Rat.add acc.(c) (Rat.mul u z))
        t.upper.(k)
  done;
  for k = t.n - 1 downto 0 do
    let p = t.prow.(k) in
    Array.iter
      (fun (r, l) ->
        if not (Rat.is_zero y.(r)) then y.(p) <- Rat.sub y.(p) (Rat.mul l y.(r)))
      t.lower.(k)
  done;
  y
