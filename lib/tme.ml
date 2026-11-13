module Box = Map.Make (Int)

type t = {
  pa : Team.plex;
  pb : Team.plex;
  po : Team.plex;
  terms : (int * int * int * Rat.t) array;
  block_a : int array;
  block_b : int array;
  live : bool array array;
}

type result = {
  lower : Rat.t;
  upper : float;
  root : float;
  nodes : int;
  x : Rat.t array;
  z : Rat.t array;
  trace : (int * float * float) list;
  warm : int;
  pivots : int;
}

let blocks (p : Team.plex) =
  let b = Array.make p.size (-1) and n = ref 0 in
  Array.to_list p.infos
  |> List.sort (fun (_, f, _) (_, f', _) -> compare f f')
  |> List.iter (fun (q, f, k) ->
      let id =
        if q = 0 then (
          incr n;
          !n - 1)
        else b.(q)
      in
      for a = 0 to k - 1 do
        b.(f + a) <- id
      done);
  (b, !n)

let make team =
  let pa, pb, po = Team.plexes team in
  let terms = Team.terms team in
  let block_a, na = blocks pa and block_b, nb = blocks pb in
  let live = Array.make_matrix na nb false in
  Array.iter
    (fun (i, j, _, _) ->
      if i > 0 && j > 0 then live.(block_a.(i)).(block_b.(j)) <- true)
    terms;
  { pa; pb; po; terms; block_a; block_b; live }

let neg = Rat.neg
let minus_one = Rat.of_int (-1)
let row coeffs rel rhs = { Lp.coeffs; rel; rhs }

let flows (p : Team.plex) var =
  Array.to_list p.infos
  |> List.filter_map (fun (q, f, n) ->
      let lhs, rhs =
        List.fold_left
          (fun (cs, c) a ->
            match var (f + a) with
            | Some v -> ((v, Rat.one) :: cs, c)
            | None -> (cs, Rat.sub c Rat.one))
          ([], Rat.zero) (List.init n Fun.id)
      in
      match var q with
      | Some v -> Some (row ((v, minus_one) :: lhs) Lp.Eq rhs)
      | None ->
          if lhs = [] then None else Some (row lhs Lp.Eq (Rat.add rhs Rat.one)))

let dual t base g =
  let po = t.po in
  let k = Array.length po.infos in
  let info = Array.make po.size 0 in
  let kids = Array.make po.size [] in
  Array.iteri
    (fun r (q, f, n) ->
      for a = 0 to n - 1 do
        info.(f + a) <- r + 1
      done;
      kids.(q) <- (r + 1) :: kids.(q))
    po.infos;
  let v r sign = [ (base + (2 * r), sign); (base + (2 * r) + 1, neg sign) ] in
  let rows =
    List.init po.size (fun s ->
        let own = v (if s = 0 then 0 else info.(s)) Rat.one in
        let down = List.concat_map (fun r -> v r minus_one) kids.(s) in
        let cs, c = g s in
        row (own @ down @ List.map (fun (j, x) -> (j, neg x)) cs) Lp.Le c)
  in
  (rows, base + (2 * (k + 1)), [ (base, Rat.one); (base + 1, minus_one) ])

let by_adversary t f =
  let g = Array.make t.po.size ([], Rat.zero) in
  Array.iter
    (fun (i, j, s, w) ->
      let cs, c = g.(s) in
      match f i j with
      | `Var (v, x) -> g.(s) <- ((v, Rat.mul w x) :: cs, c)
      | `Const x -> g.(s) <- (cs, Rat.add c (Rat.mul w x))
      | `Zero -> ())
    t.terms;
  fun s -> g.(s)

let robust ~vars obj rows =
  let rows = Array.of_list rows in
  let limit = 4 * (vars + Array.length rows) in
  let rng = Rng.make 7 in
  let rec go k =
    match Lp.bound_float ~limit ~vars obj (Array.to_list rows) with
    | Some r -> Some r
    | None when k < 8 ->
        for i = Array.length rows - 1 downto 1 do
          let j = Rng.int rng (i + 1) in
          let r = rows.(i) in
          rows.(i) <- rows.(j);
          rows.(j) <- r
        done;
        go (k + 1)
    | None -> None
  in
  go 0

type rlt = {
  vars : int;
  obj : (int * Rat.t) list;
  base : Lp.row list;
  xv : int -> int option;
  zv : int -> int option;
  w : int -> int -> int option;
  live_ij : int -> int -> bool;
}

let rlt t =
  let na = t.pa.size and nb = t.pb.size in
  let xv i = if i = 0 then None else Some (i - 1)
  and zv j = if j = 0 then None else Some (na - 1 + j - 1) in
  let idx = Hashtbl.create 1024 and next = ref (na + nb - 2) in
  for i = 1 to na - 1 do
    for j = 1 to nb - 1 do
      if t.live.(t.block_a.(i)).(t.block_b.(j)) then begin
        Hashtbl.add idx (i, j) !next;
        incr next
      end
    done
  done;
  let w i j =
    match (i, j) with
    | 0, 0 -> None
    | 0, j -> zv j
    | i, 0 -> xv i
    | i, j -> Hashtbl.find_opt idx (i, j)
  in
  let live_ij i j = i = 0 || j = 0 || t.live.(t.block_a.(i)).(t.block_b.(j)) in
  let restrict (p : Team.plex) keep =
    Team.
      {
        p with
        infos =
          Array.of_list
            (List.filter (fun (_, f, _) -> keep f) (Array.to_list p.infos));
      }
  in
  let rows_b =
    List.concat
      (List.init (na - 1) (fun i ->
           let i = i + 1 in
           flows (restrict t.pb (live_ij i)) (w i)))
  in
  let rows_a =
    List.concat
      (List.init (nb - 1) (fun j ->
           let j = j + 1 in
           flows (restrict t.pa (fun i -> live_ij i j)) (fun i -> w i j)))
  in
  let g =
    by_adversary t (fun i j ->
        match w i j with Some v -> `Var (v, Rat.one) | None -> `Const Rat.one)
  in
  let adv, vars, obj = dual t !next g in
  let base = flows t.pa xv @ flows t.pb zv @ rows_a @ rows_b @ adv in
  { vars; obj; base; xv; zv; w; live_ij }

let bound_rows t r i ?lo ?hi () =
  List.concat_map
    (fun j ->
      if not (r.live_ij i j) then []
      else
        let wij = Option.get (r.w i j) in
        let below l =
          match r.zv j with
          | None -> row [ (wij, Rat.one) ] Lp.Ge l
          | Some z -> row [ (wij, minus_one); (z, l) ] Lp.Le Rat.zero
        and above u =
          match r.zv j with
          | None -> row [ (wij, Rat.one) ] Lp.Le u
          | Some z -> row [ (wij, Rat.one); (z, neg u) ] Lp.Le Rat.zero
        in
        Option.to_list (Option.map below lo)
        @ Option.to_list (Option.map above hi))
    (List.init t.pb.size Fun.id)

let box_rows t r box =
  Box.fold
    (fun i (l, u) acc ->
      let lo = if l = 0. then None else Some (Rat.of_float l)
      and hi = if u = 1. then None else Some (Rat.of_float u) in
      bound_rows t r i ?lo ?hi () @ acc)
    box []

let trusted lp = Flp.violation lp < 1e-7

let point t r sol =
  let get = function None -> 1. | Some v -> sol.(v) in
  let x = Array.init t.pa.size (fun i -> get (r.xv i))
  and z = Array.init t.pb.size (fun j -> get (r.zv j)) in
  (x, z, fun i j -> get (r.w i j))

let warm ?dense parent rows =
  let lp = match dense with Some lp -> lp | None -> Flp.thaw parent in
  let before = Flp.pivots lp in
  match Flp.add ~limit:(20 * (Flp.rows lp + List.length rows)) lp rows with
  | Flp.Optimal when trusted lp ->
      `Solved (Flp.value lp, Flp.x lp, Some lp, Flp.pivots lp - before)
  | Infeasible -> `Empty
  | Optimal | Bound | Failed -> `Unknown

let cold t r root box =
  let extra = box_rows t r box in
  let restart =
    match root with Some root -> warm root extra | None -> `Unknown
  in
  match restart with
  | (`Solved _ | `Empty) as s -> s
  | `Unknown -> (
      let rows = r.base @ extra in
      match Flp.solve ~vars:r.vars r.obj rows with
      | lp, Flp.Optimal when trusted lp ->
          `Solved (Flp.value lp, Flp.x lp, Some lp, Flp.pivots lp)
      | lp, _ -> (
          match robust ~vars:r.vars r.obj rows with
          | Some (v, sol) -> `Solved (v, sol, None, Flp.pivots lp)
          | None -> `Unknown))

let reply_rows t (free : Team.plex) coef =
  let fv j = if j = 0 then None else Some (j - 1) in
  let g =
    by_adversary t (fun i j ->
        let s, c = coef i j in
        if Rat.is_zero c then `Zero
        else match fv s with Some v -> `Var (v, c) | None -> `Const c)
  in
  let adv, vars, obj = dual t (free.size - 1) g in
  (vars, obj, flows free fv @ adv)

let project (p : Team.plex) x =
  let r = Array.make p.size 0. in
  r.(0) <- 1.;
  Array.to_list p.infos
  |> List.sort (fun (_, f, _) (_, f', _) -> compare f f')
  |> List.iter (fun (q, f, n) ->
      let probs = Array.init n (fun a -> Float.max 0. x.(f + a)) in
      let total = Array.fold_left ( +. ) 0. probs in
      Array.iteri
        (fun a pr ->
          r.(f + a) <-
            (r.(q) *. if total <= 1e-12 then 1. /. float n else pr /. total))
        probs);
  r

let reply t (free : Team.plex) coef =
  let vars, obj, rows = reply_rows t free coef in
  let plan x =
    project free
      (Array.init free.size (fun j -> if j = 0 then 1. else x.(j - 1)))
  in
  match Flp.solve ~vars obj rows with
  | lp, Flp.Optimal when trusted lp -> plan (Flp.x lp)
  | _ -> (
      match robust ~vars obj rows with
      | Some (_, x) -> plan x
      | None -> (
          match Lp.maximize ~vars obj rows with
          | Lp.Optimal { x; _ } -> plan (Array.map Rat.to_float x)
          | Infeasible | Unbounded -> invalid_arg "Tme: reply LP"))

let reply_b t x = reply t t.pb (fun i j -> (j, Rat.of_float x.(i)))
let reply_a t z = reply t t.pa (fun i j -> (i, Rat.of_float z.(j)))

let worst t x z =
  let g = Array.make t.po.size 0. in
  Array.iter
    (fun (i, j, s, w) -> g.(s) <- g.(s) +. (Rat.to_float w *. x.(i) *. z.(j)))
    t.terms;
  Array.to_list t.po.infos
  |> List.sort (fun (_, f, _) (_, f', _) -> compare f' f)
  |> List.iter (fun (q, f, n) ->
      let m = ref infinity in
      for a = 0 to n - 1 do
        m := Float.min !m g.(f + a)
      done;
      g.(q) <- g.(q) +. !m);
  g.(0)

let climb t x =
  let rec go k x z v =
    let x' = reply_a t z in
    let z' = reply_b t x' in
    let v' = worst t x' z' in
    if v' <= v +. 1e-12 then (v, x, z)
    else if k < 20 then go (k + 1) x' z' v'
    else (v', x', z')
  in
  let x = project t.pa x in
  let z = reply_b t x in
  go 0 x z (worst t x z)

let near p =
  let rec go d =
    if d > 1000 then
      Rat.of_ints (Float.to_int (Float.round (p *. 1048576.))) 1048576
    else
      let n = Float.round (p *. float d) in
      if Float.abs ((n /. float d) -. p) < 1e-9 then
        Rat.of_ints (Float.to_int n) d
      else go (d + 1)
  in
  go 1

let down p = Rat.of_ints (Float.to_int (Float.of_int 1048576 *. p)) 1048576

let exact_plan (p : Team.plex) x =
  let x = project p x in
  let r = Array.make p.size Rat.zero in
  r.(0) <- Rat.one;
  Array.to_list p.infos
  |> List.sort (fun (_, f, _) (_, f', _) -> compare f f')
  |> List.iter (fun (q, f, n) ->
      let probs =
        Array.init n (fun a ->
            if x.(q) <= 1e-12 then 1. /. float n else x.(f + a) /. x.(q))
      in
      let rounded round =
        let ps = Array.map round probs in
        let sum = Array.fold_left Rat.add Rat.zero (Array.sub ps 0 (n - 1)) in
        ps.(n - 1) <- Rat.sub Rat.one sum;
        ps
      in
      let ps = rounded near in
      let ps = if Rat.sign ps.(n - 1) < 0 then rounded down else ps in
      Array.iteri (fun a pr -> r.(f + a) <- Rat.mul r.(q) pr) ps);
  r

let exact_reply t (free : Team.plex) coef =
  let vars, obj, rows = reply_rows t free coef in
  match Lp.maximize ~vars obj rows with
  | Lp.Optimal { value; x; _ } ->
      ( value,
        Array.init free.size (fun j -> if j = 0 then Rat.one else x.(j - 1)) )
  | Infeasible | Unbounded -> invalid_arg "Tme: reply LP"

let certify t x z =
  let xr = exact_plan t.pa x and zr = exact_plan t.pb z in
  let vb, zb = exact_reply t t.pb (fun i j -> (j, xr.(i))) in
  let va, xa = exact_reply t t.pa (fun i j -> (i, zr.(j))) in
  if Rat.compare vb va >= 0 then (vb, xr, zb) else (va, xa, zr)

let error t x z w =
  let e = Array.make t.pa.size 0. in
  Array.iter
    (fun (i, j, _, c) ->
      if i > 0 && j > 0 then
        e.(i) <-
          e.(i)
          +. Float.abs (Rat.to_float c)
             *. Float.abs (w i j -. (x.(i) *. z.(j))))
    t.terms;
  e

let range box i = Option.value ~default:(0., 1.) (Box.find_opt i box)

let candidates k box x e =
  let inside i =
    let l, u = range box i in
    let d = 1e-3 *. (u -. l) in
    x.(i) > l +. d && x.(i) < u -. d
  in
  let ranked ok =
    List.init (Array.length e) Fun.id
    |> List.filter (fun i -> e.(i) > 1e-9 && ok i)
    |> List.stable_sort (fun i j -> Float.compare e.(j) e.(i))
  in
  match ranked inside with
  | [] -> (
      match ranked (fun _ -> true) with
      | [] -> []
      | i :: _ ->
          let l, u = range box i in
          [ (i, (l +. u) /. 2.) ])
  | is ->
      List.filteri (fun n _ -> n < k) is
      |> List.map (fun i ->
          let l, u = range box i in
          (i, (0.75 *. x.(i)) +. (0.125 *. (l +. u))))

let split box i m =
  let l, u = range box i in
  (Box.add i (l, m) box, Box.add i (m, u) box)

module Frontier = Set.Make (struct
  type t = float * int

  let compare (a, i) (b, j) =
    match Float.compare b a with 0 -> compare j i | c -> c
end)

let keep = 48

let solve ?(gap = 1e-6) ?(limit = 20_000) ?(strong = 1) team =
  let t = make team in
  let r = rlt t in
  let boxes = Hashtbl.create 1024 in
  let best = ref (neg_infinity, [||], [||]) and held = ref neg_infinity in
  let stored = ref Frontier.empty in
  let warmed = ref 0 and pivots = ref 0 and root_lp = ref None in
  let spare = ref None in
  let lower () =
    let b, _, _ = !best in
    b
  in
  let offer ((v, _, _) as c) = if v > lower () then best := c in
  let hold v = held := Float.max !held v in
  let remember key =
    stored := Frontier.add key !stored;
    if Frontier.cardinal !stored > keep then begin
      let ((_, id) as last) = Frontier.max_elt !stored in
      stored := Frontier.remove last !stored;
      let box, _ = Hashtbl.find boxes id in
      Hashtbl.replace boxes id (box, None)
    end
  in
  let relax box start =
    let attempt =
      match start with
      | Some (parent, rows) ->
          let dense =
            match !spare with
            | Some (f, lp) when f == parent ->
                spare := None;
                Some lp
            | _ -> None
          in
          warm ?dense parent rows
      | None -> `Unknown
    in
    (match attempt with `Solved _ -> incr warmed | `Empty | `Unknown -> ());
    let s =
      match attempt with
      | (`Solved _ | `Empty) as s -> s
      | `Unknown -> cold t r !root_lp box
    in
    match s with
    | `Solved (v, sol, lp, n) ->
        pivots := !pivots + n;
        let x, z, w = point t r sol in
        `Solved (v, x, z, w, lp)
    | (`Empty | `Unknown) as s -> s
  in
  let rec loop q id nodes root trace =
    match Frontier.min_elt_opt q with
    | Some ((ub, k) as top) when ub > lower () +. gap && nodes < limit -> (
        let q = Frontier.remove top q in
        let box, start = Hashtbl.find boxes k in
        stored := Frontier.remove top !stored;
        Hashtbl.remove boxes k;
        let nodes = nodes + 1 in
        match relax box start with
        | `Empty -> loop q id nodes root trace
        | `Unknown ->
            hold ub;
            loop q id nodes root trace
        | `Solved (v, x, z, w, lp) ->
            let root = if nodes = 1 then v else root in
            let v = Float.min v ub in
            if v <= lower () +. gap then begin
              hold v;
              loop q id nodes root trace
            end
            else (
              offer (climb t x);
              let top =
                Option.fold ~none:v ~some:(fun (u, _) -> Float.max u v)
              in
              let trace =
                (nodes, lower (), Float.max !held (top (Frontier.min_elt_opt q)))
                :: trace
              in
              match candidates strong box x (error t x z w) with
              | [] ->
                  hold v;
                  loop q id nodes root trace
              | first :: _ as cands ->
                  let frozen = Option.map Flp.freeze lp in
                  if nodes = 1 then root_lp := frozen;
                  spare :=
                    Option.bind frozen (fun f ->
                        Option.map (fun lp -> (f, lp)) lp);
                  let sides (i, m) =
                    let m = Rat.of_float m in
                    (bound_rows t r i ~hi:m (), bound_rows t r i ~lo:m ())
                  in
                  let probe f rows =
                    match warm f rows with
                    | `Solved (u, _, _, n) ->
                        pivots := !pivots + n;
                        Float.min u v
                    | `Empty -> neg_infinity
                    | `Unknown -> v
                  in
                  let gain u = Float.max (v -. u) 1e-9 in
                  let (i, m), (below, above), (vl, vr) =
                    match frozen with
                    | Some f when List.length cands > 1 ->
                        List.map
                          (fun c ->
                            let ((lo, hi) as rows) = sides c in
                            (c, rows, (probe f lo, probe f hi)))
                          cands
                        |> List.fold_left
                             (fun best ((_, _, (a, b)) as c) ->
                               match best with
                               | Some (_, _, (a', b'))
                                 when gain a' *. gain b' >= gain a *. gain b ->
                                   best
                               | _ -> Some c)
                             None
                        |> Option.get
                    | _ -> (first, sides first, (v, v))
                  in
                  let left, right = split box i m in
                  let push (q, id) box rows u =
                    if u <= lower () +. gap then begin
                      hold u;
                      (q, id + 1)
                    end
                    else begin
                      Hashtbl.add boxes id
                        (box, Option.map (fun f -> (f, rows)) frozen);
                      if Option.is_some frozen then remember (u, id);
                      (Frontier.add (u, id) q, id + 1)
                    end
                  in
                  let q, id = push (q, id) left below vl in
                  let q, id = push (q, id) right above vr in
                  loop q id nodes root trace))
    | top ->
        let open_ub =
          match top with Some (ub, _) -> ub | None -> neg_infinity
        in
        (nodes, root, Float.max open_ub !held, List.rev trace)
  in
  Hashtbl.add boxes 0 (Box.empty, None);
  let nodes, root, upper, trace =
    loop (Frontier.singleton (infinity, 0)) 1 0 neg_infinity []
  in
  let _, x, z = !best in
  let lower, x, z = certify t x z in
  {
    lower;
    upper = Float.max upper (Rat.to_float lower);
    root;
    nodes;
    x;
    z;
    trace;
    warm = !warmed;
    pivots = !pivots;
  }
