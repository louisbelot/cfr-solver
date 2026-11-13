type status = Optimal | Bound | Infeasible | Failed

type line = {
  idx : int array;
  coef : float array;
  rel : Lp.relation;
  bound : float;
}

type t = {
  vars : int;
  cost : float array;
  mutable m : int;
  mutable n : int;
  mutable tab : float array array;
  mutable rhs : float array;
  mutable obj : float array;
  mutable z : float;
  mutable basis : int array;
  mutable unit : int array;
  mutable b0 : float array;
  mutable closed : bool array;
  mutable lines : line list;
  mutable norm : line array;
  mutable nz : int array;
  mutable pivots : int;
}

type frozen = { shell : t; sparse : (int array * float array) array }

let blank = { idx = [||]; coef = [||]; rel = Lp.Le; bound = 0. }
let drop = 1e-11
let piv = 1e-9
let tol = 1e-9
let feas = 1e-7

let line_of (r : Lp.row) =
  let tbl = Hashtbl.create 8 in
  List.iter
    (fun (j, c) ->
      let c = Rat.to_float c in
      Hashtbl.replace tbl j
        (c +. Option.value ~default:0. (Hashtbl.find_opt tbl j)))
    r.coeffs;
  let ps =
    Hashtbl.fold (fun j c acc -> if c = 0. then acc else (j, c) :: acc) tbl []
    |> List.sort compare
  in
  {
    idx = Array.of_list (List.map fst ps);
    coef = Array.of_list (List.map snd ps);
    rel = r.rel;
    bound = Rat.to_float r.rhs;
  }

let negate l =
  {
    l with
    coef = Array.map Float.neg l.coef;
    bound = -.l.bound;
    rel = (match l.rel with Lp.Le -> Lp.Ge | Ge -> Le | Eq -> Eq);
  }

let grow t cols rows =
  let cap = Array.length t.obj in
  if cols > cap then begin
    let cap' = max cols (2 * cap) in
    let widen a fill =
      let b = Array.make cap' fill in
      Array.blit a 0 b 0 (Array.length a);
      b
    in
    t.tab <- Array.map (fun r -> widen r 0.) t.tab;
    t.obj <- widen t.obj 0.;
    t.closed <- widen t.closed false;
    t.nz <- Array.make cap' 0
  end;
  if rows > Array.length t.tab then begin
    let cap' = max rows (2 * Array.length t.tab) in
    let width = Array.length t.obj in
    let old = t.tab in
    t.tab <-
      Array.init cap' (fun i ->
          if i < Array.length old then old.(i) else Array.make width 0.);
    let widen a fill =
      let b = Array.make cap' fill in
      Array.blit a 0 b 0 (Array.length a);
      b
    in
    t.rhs <- widen t.rhs 0.;
    t.basis <- widen t.basis 0;
    t.unit <- widen t.unit 0;
    t.b0 <- widen t.b0 0.;
    t.norm <- widen t.norm blank
  end

let pivot t r s =
  let row = t.tab.(r) in
  let p = row.(s) in
  let nz = t.nz and k = ref 0 in
  for j = 0 to t.n - 1 do
    let v = row.(j) in
    if v <> 0. then begin
      let v = v /. p in
      if Float.abs v < drop then row.(j) <- 0.
      else begin
        row.(j) <- v;
        nz.(!k) <- j;
        incr k
      end
    end
  done;
  row.(s) <- 1.;
  let br = t.rhs.(r) /. p in
  t.rhs.(r) <- br;
  let cnt = !k in
  let elim o =
    let f = o.(s) in
    if f <> 0. then begin
      for q = 0 to cnt - 1 do
        let j = nz.(q) in
        let v = o.(j) -. (f *. row.(j)) in
        o.(j) <- (if Float.abs v < drop then 0. else v)
      done;
      o.(s) <- 0.
    end;
    f
  in
  for i = 0 to t.m - 1 do
    if i <> r then begin
      let f = elim t.tab.(i) in
      if f <> 0. then t.rhs.(i) <- t.rhs.(i) -. (f *. br)
    end
  done;
  let f = elim t.obj in
  t.z <- t.z -. (f *. br);
  t.basis.(r) <- s;
  t.pivots <- t.pivots + 1

let price t cost =
  Array.fill t.obj 0 (Array.length t.obj) 0.;
  for j = 0 to t.n - 1 do
    t.obj.(j) <- -.cost j
  done;
  t.z <- 0.;
  for i = 0 to t.m - 1 do
    let c = cost t.basis.(i) in
    if c <> 0. then begin
      let r = t.tab.(i) in
      for j = 0 to t.n - 1 do
        if r.(j) <> 0. then t.obj.(j) <- t.obj.(j) +. (c *. r.(j))
      done;
      t.z <- t.z +. (c *. t.rhs.(i))
    end
  done

let entering t ~bland =
  let best = ref (-1) in
  (try
     for j = 0 to t.n - 1 do
       let d = t.obj.(j) in
       if d < -.tol && not t.closed.(j) then
         if bland then begin
           best := j;
           raise Exit
         end
         else if !best < 0 || d < t.obj.(!best) then best := j
     done
   with Exit -> ());
  !best

let leaving t s =
  let bound = ref infinity in
  for i = 0 to t.m - 1 do
    let a = t.tab.(i).(s) in
    if a > piv then
      bound := Float.min !bound ((Float.max t.rhs.(i) 0. +. tol) /. a)
  done;
  if !bound = infinity then -1
  else begin
    let best = ref (-1) in
    for i = 0 to t.m - 1 do
      let a = t.tab.(i).(s) in
      if
        a > piv
        && Float.max t.rhs.(i) 0. /. a <= !bound
        && (!best < 0 || a > t.tab.(!best).(s))
      then best := i
    done;
    !best
  end

let primal t ~limit =
  let rec go stall =
    let s = entering t ~bland:(stall > 50) in
    if s < 0 then `Optimal
    else if t.pivots >= limit then `Stopped
    else
      let r = leaving t s in
      if r < 0 then `Unbounded
      else
        let before = t.z in
        pivot t r s;
        go (if Float.abs (t.z -. before) < 1e-12 then stall + 1 else 0)
  in
  go 0

let farkas t r =
  let row = t.tab.(r) and col = Array.make t.vars 0. in
  let yb = ref 0. and signs = ref true in
  for k = 0 to t.m - 1 do
    let y = row.(t.unit.(k)) in
    if y <> 0. then begin
      let l = t.norm.(k) in
      (match l.rel with
      | Lp.Le -> if y < -.piv then signs := false
      | Ge -> if y > piv then signs := false
      | Eq -> ());
      Array.iteri (fun q j -> col.(j) <- col.(j) +. (y *. l.coef.(q))) l.idx;
      yb := !yb +. (y *. l.bound)
    end
  done;
  !signs && !yb < -.feas && Array.for_all (fun c -> c >= -.piv) col

let dual t ~limit =
  let rec go () =
    let r = ref (-1) in
    for i = 0 to t.m - 1 do
      if t.rhs.(i) < -.feas && (!r < 0 || t.rhs.(i) < t.rhs.(!r)) then r := i
    done;
    if !r < 0 then Optimal
    else if t.pivots >= limit then Bound
    else begin
      let row = t.tab.(!r) and bound = ref infinity in
      for j = 0 to t.n - 1 do
        let a = row.(j) in
        if a < -.piv && not t.closed.(j) then
          bound := Float.min !bound ((Float.max t.obj.(j) 0. +. tol) /. -.a)
      done;
      if !bound = infinity then if farkas t !r then Infeasible else Failed
      else begin
        let s = ref (-1) in
        for j = 0 to t.n - 1 do
          let a = row.(j) in
          if
            a < -.piv
            && (not t.closed.(j))
            && Float.max t.obj.(j) 0. /. -.a <= !bound
            && (!s < 0 || a < row.(!s))
          then s := j
        done;
        pivot t !r !s;
        go ()
      end
    end
  in
  go ()

let priced t =
  let ok = ref true in
  for j = 0 to t.n - 1 do
    if t.obj.(j) < -.feas && not t.closed.(j) then ok := false
  done;
  !ok

let x t =
  let x = Array.make t.vars 0. in
  for i = 0 to t.m - 1 do
    let b = t.basis.(i) in
    if b < t.vars then x.(b) <- t.rhs.(i)
  done;
  x

let violation t =
  let x = x t in
  let worst = ref (Array.fold_left (fun w v -> Float.max w (-.v)) 0. x) in
  List.iter
    (fun l ->
      let a = ref 0. in
      Array.iteri (fun k j -> a := !a +. (l.coef.(k) *. x.(j))) l.idx;
      let d =
        match l.rel with
        | Lp.Le -> !a -. l.bound
        | Ge -> l.bound -. !a
        | Eq -> Float.abs (!a -. l.bound)
      in
      worst := Float.max !worst d)
    t.lines;
  !worst

let refresh t =
  for i = 0 to t.m - 1 do
    let r = t.tab.(i) and v = ref 0. in
    for k = 0 to t.m - 1 do
      let c = r.(t.unit.(k)) in
      if c <> 0. then v := !v +. (c *. t.b0.(k))
    done;
    t.rhs.(i) <- !v
  done;
  let v = ref 0. in
  for k = 0 to t.m - 1 do
    v := !v +. (t.obj.(t.unit.(k)) *. t.b0.(k))
  done;
  t.z <- !v

let rec finish t ~limit k =
  match dual t ~limit with
  | Optimal -> (
      match primal t ~limit with
      | `Optimal ->
          let clean = ref true in
          for i = 0 to t.m - 1 do
            if t.rhs.(i) < -.feas then clean := false
          done;
          if !clean && violation t <= feas then Optimal
          else if k < 3 then begin
            refresh t;
            finish t ~limit (k + 1)
          end
          else Failed
      | `Unbounded | `Stopped -> Failed)
  | Bound -> if priced t then Bound else Failed
  | (Infeasible | Failed) as s -> s

let shift j = float (1000 + ((j + 1) * 7919 mod 1000)) *. 1e-9

let solve ?limit ~vars objective rows =
  let lines = List.map line_of rows in
  let moved =
    List.map
      (fun l ->
        let b = ref l.bound in
        Array.iteri (fun k j -> b := !b +. (l.coef.(k) *. shift j)) l.idx;
        if !b < 0. then (negate l, -. !b) else (l, !b))
      lines
    |> Array.of_list
  in
  let m = Array.length moved in
  let extra =
    Array.fold_left
      (fun n ((l : line), _) ->
        n + match l.rel with Lp.Ge -> 2 | Le | Eq -> 1)
      0 moved
  in
  let n = vars + extra in
  let limit = Option.value limit ~default:(20 * (m + n)) in
  let cost = Array.make vars 0. in
  List.iter
    (fun (j, c) ->
      if j < 0 || j >= vars then invalid_arg "Flp.solve: variable";
      cost.(j) <- cost.(j) +. Rat.to_float c)
    objective;
  let t =
    {
      vars;
      cost;
      m;
      n;
      tab = Array.init m (fun _ -> Array.make n 0.);
      rhs = Array.map snd moved;
      obj = Array.make n 0.;
      z = 0.;
      basis = Array.make m 0;
      unit = Array.make m 0;
      b0 = Array.map (fun ((l : line), _) -> l.bound) moved;
      closed = Array.make n false;
      lines;
      norm = Array.map fst moved;
      nz = Array.make n 0;
      pivots = 0;
    }
  in
  let next = ref vars and arts = ref [] in
  Array.iteri
    (fun i ((l : line), _) ->
      let r = t.tab.(i) in
      Array.iteri (fun k j -> r.(j) <- r.(j) +. l.coef.(k)) l.idx;
      let col () =
        let c = !next in
        incr next;
        c
      in
      let u =
        match l.rel with
        | Le ->
            let s = col () in
            r.(s) <- 1.;
            s
        | Ge ->
            let s = col () in
            r.(s) <- -1.;
            let a = col () in
            r.(a) <- 1.;
            arts := a :: !arts;
            a
        | Eq ->
            let a = col () in
            r.(a) <- 1.;
            arts := a :: !arts;
            a
      in
      t.basis.(i) <- u;
      t.unit.(i) <- u)
    moved;
  let art = Array.make n false in
  List.iter (fun a -> art.(a) <- true) !arts;
  price t (fun j -> if art.(j) then -1. else 0.);
  match primal t ~limit with
  | `Optimal when t.z >= -1e-6 -> (
      Array.iteri (fun j a -> if a then t.closed.(j) <- true) art;
      for i = 0 to m - 1 do
        if art.(t.basis.(i)) then
          let r = t.tab.(i) in
          let rec find j =
            if j < n then
              if (not art.(j)) && Float.abs r.(j) > piv then pivot t i j
              else find (j + 1)
          in
          find 0
      done;
      price t (fun j -> if j < vars then cost.(j) else 0.);
      match primal t ~limit with
      | `Optimal ->
          refresh t;
          (t, finish t ~limit 0)
      | `Unbounded | `Stopped -> (t, Failed))
  | `Optimal -> (t, Infeasible)
  | `Unbounded | `Stopped -> (t, Failed)

let add ?limit t rows =
  let added = List.map line_of rows in
  let fresh =
    List.map
      (fun l ->
        match l.rel with
        | Lp.Le -> l
        | Ge -> negate l
        | Eq -> invalid_arg "Flp.add: equality")
      added
  in
  let k = List.length fresh in
  grow t (t.n + k) (t.m + k);
  let pos = Array.make t.n (-1) in
  for i = 0 to t.m - 1 do
    pos.(t.basis.(i)) <- i
  done;
  List.iter
    (fun l ->
      let i = t.m and s = t.n in
      t.m <- i + 1;
      t.n <- s + 1;
      let r = t.tab.(i) in
      Array.fill r 0 (Array.length r) 0.;
      r.(s) <- 1.;
      let b = ref l.bound in
      Array.iteri
        (fun q j ->
          let c = l.coef.(q) in
          let p = if j < Array.length pos then pos.(j) else -1 in
          if p < 0 then r.(j) <- r.(j) +. c
          else begin
            let src = t.tab.(p) in
            for j' = 0 to s - 1 do
              let v = src.(j') in
              if v <> 0. then r.(j') <- r.(j') -. (c *. v)
            done;
            r.(j) <- r.(j) +. c;
            b := !b -. (c *. t.rhs.(p))
          end)
        l.idx;
      for j = 0 to s - 1 do
        if Float.abs r.(j) < drop then r.(j) <- 0.
      done;
      t.rhs.(i) <- !b;
      t.basis.(i) <- s;
      t.unit.(i) <- s;
      t.b0.(i) <- l.bound;
      t.norm.(i) <- l;
      t.obj.(s) <- 0.;
      t.closed.(s) <- false)
    fresh;
  t.lines <- List.rev_append added t.lines;
  let limit = Option.value limit ~default:max_int in
  finish t ~limit:(if limit = max_int then max_int else t.pivots + limit) 0

let value t = t.z
let rows t = t.m
let pivots t = t.pivots

let freeze t =
  let sparse =
    Array.init t.m (fun i ->
        let r = t.tab.(i) in
        let idx = ref [] and v = ref [] in
        for j = t.n - 1 downto 0 do
          if r.(j) <> 0. then begin
            idx := j :: !idx;
            v := r.(j) :: !v
          end
        done;
        (Array.of_list !idx, Array.of_list !v))
  in
  {
    shell =
      {
        t with
        tab = [||];
        rhs = Array.sub t.rhs 0 t.m;
        obj = Array.sub t.obj 0 t.n;
        basis = Array.sub t.basis 0 t.m;
        unit = Array.sub t.unit 0 t.m;
        b0 = Array.sub t.b0 0 t.m;
        norm = Array.sub t.norm 0 t.m;
        closed = Array.sub t.closed 0 t.n;
        nz = [||];
      };
    sparse;
  }

let thaw { shell; sparse } =
  let n = shell.n in
  {
    shell with
    tab =
      Array.map
        (fun (idx, v) ->
          let r = Array.make n 0. in
          Array.iteri (fun k j -> r.(j) <- v.(k)) idx;
          r)
        sparse;
    rhs = Array.copy shell.rhs;
    obj = Array.copy shell.obj;
    basis = Array.copy shell.basis;
    unit = Array.copy shell.unit;
    b0 = Array.copy shell.b0;
    norm = Array.copy shell.norm;
    closed = Array.copy shell.closed;
    nz = Array.make n 0;
  }

let size { sparse; _ } =
  Array.fold_left (fun a (idx, _) -> a + Array.length idx) 0 sparse
