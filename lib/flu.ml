type t = {
  n : int;
  prow : int array;
  pcol : int array;
  diag : float array;
  upper : (int * float) array array;
  lower : (int * float) array array;
}

let threshold = 0.1
let drop = 1e-14
let tiny = 1e-11
let search = 4

exception Singular

type work = {
  rows : (int, float) Hashtbl.t array;
  cols : (int, unit) Hashtbl.t array;
  head : int array;
  next : int array;
  prev : int array;
  count : int array;
}

let unlink w j =
  let p = w.prev.(j) and n = w.next.(j) in
  if p >= 0 then w.next.(p) <- n else w.head.(w.count.(j)) <- n;
  if n >= 0 then w.prev.(n) <- p

let link w j c =
  w.count.(j) <- c;
  w.prev.(j) <- -1;
  w.next.(j) <- w.head.(c);
  if w.head.(c) >= 0 then w.prev.(w.head.(c)) <- j;
  w.head.(c) <- j

let move w j c =
  unlink w j;
  link w j c

let set w i j v =
  if Float.abs v < drop then (
    if Hashtbl.mem w.rows.(i) j then (
      Hashtbl.remove w.rows.(i) j;
      Hashtbl.remove w.cols.(j) i;
      move w j (w.count.(j) - 1)))
  else (
    if not (Hashtbl.mem w.rows.(i) j) then (
      Hashtbl.replace w.cols.(j) i ();
      move w j (w.count.(j) + 1));
    Hashtbl.replace w.rows.(i) j v)

let colmax w j =
  Hashtbl.fold
    (fun i () m -> Float.max m (Float.abs (Hashtbl.find w.rows.(i) j)))
    w.cols.(j) 0.

let consider w c j best cost seen =
  let big = colmax w j in
  if big < tiny then raise Singular;
  let found = ref false in
  Hashtbl.iter
    (fun i () ->
      if Float.abs (Hashtbl.find w.rows.(i) j) >= threshold *. big then (
        found := true;
        let k = (Hashtbl.length w.rows.(i) - 1) * (c - 1) in
        if k < !cost then (
          best := (i, j);
          cost := k)))
    w.cols.(j);
  if !found then incr seen;
  if !cost = 0 then raise Exit

let choose n w =
  if w.head.(0) >= 0 then raise Singular;
  let best = ref (-1, -1) and cost = ref max_int and seen = ref 0 in
  (try
     for c = 1 to n do
       if !seen >= search || (!cost < max_int && !cost <= (c - 1) * (c - 1))
       then raise Exit;
       let rec walk j =
         if j >= 0 then (
           consider w c j best cost seen;
           walk w.next.(j))
       in
       walk w.head.(c)
     done
   with Exit -> ());
  if fst !best < 0 then raise Singular;
  !best

let factor n columns =
  let w =
    {
      rows = Array.init n (fun _ -> Hashtbl.create 8);
      cols = Array.init n (fun _ -> Hashtbl.create 8);
      head = Array.make (n + 1) (-1);
      next = Array.make n (-1);
      prev = Array.make n (-1);
      count = Array.make n 0;
    }
  in
  Array.iteri (fun j _ -> link w j 0) columns;
  Array.iteri
    (fun j col ->
      List.iter
        (fun (i, v) ->
          set w i j
            (v +. Option.value (Hashtbl.find_opt w.rows.(i) j) ~default:0.))
        col)
    columns;
  let prow = Array.make n 0 and pcol = Array.make n 0 in
  let diag = Array.make n 0. in
  let upper = Array.make n [||] and lower = Array.make n [||] in
  try
    for k = 0 to n - 1 do
      let i, j = choose n w in
      let p = Hashtbl.find w.rows.(i) j in
      let row =
        Hashtbl.fold
          (fun c v acc -> if c = j then acc else (c, v) :: acc)
          w.rows.(i) []
      in
      let others =
        Hashtbl.fold
          (fun r () acc -> if r = i then acc else r :: acc)
          w.cols.(j) []
      in
      List.iter (fun (c, _) -> set w i c 0.) row;
      set w i j 0.;
      let mults =
        List.map
          (fun r ->
            let l = Hashtbl.find w.rows.(r) j /. p in
            set w r j 0.;
            List.iter
              (fun (c, v) ->
                let old =
                  Option.value (Hashtbl.find_opt w.rows.(r) c) ~default:0.
                in
                set w r c (old -. (l *. v)))
              row;
            (r, l))
          others
      in
      unlink w j;
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
    if v <> 0. then
      Array.iter (fun (r, l) -> w.(r) <- w.(r) -. (l *. v)) t.lower.(k)
  done;
  let x = Array.make t.n 0. in
  for k = t.n - 1 downto 0 do
    let s =
      Array.fold_left
        (fun acc (c, u) -> acc -. (u *. x.(c)))
        w.(t.prow.(k))
        t.upper.(k)
    in
    x.(t.pcol.(k)) <- s /. t.diag.(k)
  done;
  x

let solve_t t c =
  let acc = Array.make t.n 0. and y = Array.make t.n 0. in
  for k = 0 to t.n - 1 do
    let j = t.pcol.(k) in
    let z = (c.(j) -. acc.(j)) /. t.diag.(k) in
    y.(t.prow.(k)) <- z;
    if z <> 0. then
      Array.iter (fun (c, u) -> acc.(c) <- acc.(c) +. (u *. z)) t.upper.(k)
  done;
  for k = t.n - 1 downto 0 do
    let p = t.prow.(k) in
    Array.iter
      (fun (r, l) -> if y.(r) <> 0. then y.(p) <- y.(p) -. (l *. y.(r)))
      t.lower.(k)
  done;
  y

let nonzeros t =
  Array.fold_left (fun a u -> a + Array.length u) t.n t.upper
  + Array.fold_left (fun a l -> a + Array.length l) 0 t.lower
