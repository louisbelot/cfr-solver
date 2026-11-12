type relation = Le | Eq | Ge
type row = { coeffs : (int * Rat.t) list; rel : relation; rhs : Rat.t }

type result =
  | Optimal of { value : Rat.t; x : Rat.t array; pivots : int }
  | Infeasible
  | Unbounded

module type FIELD = sig
  type t

  val zero : t
  val one : t
  val tol : t
  val of_rat : Rat.t -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val sign : t -> int
  val compare : t -> t -> int
end

module Exact = struct
  include Rat

  let tol = zero
  let of_rat = Fun.id
end

module Approx = struct
  type t = float

  let eps = 1e-9
  let tol = 1e-9
  let zero = 0.
  let one = 1.
  let of_rat = Rat.to_float
  let add = ( +. )

  let sub a b =
    let d = a -. b in
    if Float.abs d < eps then 0. else d

  let mul = ( *. )
  let div = ( /. )
  let sign x = if x > eps then 1 else if x < -.eps then -1 else 0
  let compare = Float.compare
end

module Simplex (F : FIELD) = struct
  type t = {
    rows : F.t array array;
    basis : int array;
    obj : F.t array;
    cols : int;
    init : int array;
    mutable pivots : int;
    mutable harris : bool;
  }

  let zero x = F.sign x = 0
  let rhs t r = r.(t.cols)

  let pivot t r s =
    let row = t.rows.(r) in
    let p = row.(s) in
    if F.compare p F.one <> 0 then
      Array.iteri (fun k v -> if not (zero v) then row.(k) <- F.div v p) row;
    row.(s) <- F.one;
    let nz =
      Array.of_list
        (List.filter
           (fun k -> not (zero row.(k)))
           (List.init (t.cols + 1) Fun.id))
    in
    let eliminate other =
      let f = other.(s) in
      if not (zero f) then (
        Array.iter (fun k -> other.(k) <- F.sub other.(k) (F.mul f row.(k))) nz;
        other.(s) <- F.zero)
    in
    Array.iteri (fun i other -> if i <> r then eliminate other) t.rows;
    eliminate t.obj;
    t.basis.(r) <- s;
    t.pivots <- t.pivots + 1

  let entering t ~bland ~allowed =
    let best = ref (-1) in
    (try
       for j = 0 to allowed - 1 do
         let d = t.obj.(j) in
         if F.sign d < 0 then
           if bland then (
             best := j;
             raise Exit)
           else if !best < 0 || F.compare d t.obj.(!best) < 0 then best := j
       done
     with Exit -> ());
    !best

  let lex t s i j =
    let ri = t.rows.(i) and rj = t.rows.(j) in
    let rec go k =
      if k = Array.length t.init then 0
      else
        let c = t.init.(k) in
        let d = F.compare (F.div ri.(c) ri.(s)) (F.div rj.(c) rj.(s)) in
        if d <> 0 then d else go (k + 1)
    in
    go 0

  let lexical t s =
    let best = ref (-1) and ratio = ref F.zero in
    Array.iteri
      (fun i r ->
        if F.sign r.(s) > 0 then
          let q = F.div (rhs t r) r.(s) in
          let c = if !best < 0 then -1 else F.compare q !ratio in
          if c < 0 || (c = 0 && lex t s i !best < 0) then (
            best := i;
            ratio := q))
      t.rows;
    (!best, !ratio)

  let harris t s =
    let bound = ref None in
    Array.iter
      (fun r ->
        if F.sign r.(s) > 0 then
          let q = F.div (F.add (rhs t r) F.tol) r.(s) in
          match !bound with
          | Some b when F.compare b q <= 0 -> ()
          | _ -> bound := Some q)
      t.rows;
    match !bound with
    | None -> (-1, F.zero)
    | Some b ->
        let best = ref (-1) in
        Array.iteri
          (fun i r ->
            if
              F.sign r.(s) > 0
              && F.compare (F.div (rhs t r) r.(s)) b <= 0
              && (!best < 0 || F.compare r.(s) t.rows.(!best).(s) > 0)
            then best := i)
          t.rows;
        let r = t.rows.(!best) in
        (!best, F.div (rhs t r) r.(s))

  let leaving t s = if t.harris then harris t s else lexical t s

  let run ?(limit = max_int) t ~bland ~allowed =
    let rec loop stall =
      let s = entering t ~bland:(bland || stall > 50) ~allowed in
      if s < 0 then `Optimal
      else if t.pivots >= limit then `Stopped
      else
        let r, ratio = leaving t s in
        if r < 0 then `Unbounded
        else (
          pivot t r s;
          loop (if zero ratio then stall + 1 else 0))
    in
    loop 0

  let price t cost =
    Array.fill t.obj 0 (t.cols + 1) F.zero;
    for j = 0 to t.cols - 1 do
      t.obj.(j) <- F.sub F.zero (cost j)
    done;
    Array.iteri
      (fun i r ->
        let c = cost t.basis.(i) in
        if not (zero c) then
          Array.iteri (fun k v -> t.obj.(k) <- F.add t.obj.(k) (F.mul c v)) r)
      t.rows

  let setup ~vars rows =
    let m = Array.length rows in
    let slacks =
      Array.fold_left (fun n r -> if r.rel = Eq then n else n + 1) 0 rows
    in
    let first_art = vars + slacks in
    let arts =
      Array.fold_left (fun n r -> if r.rel = Le then n else n + 1) 0 rows
    in
    let cols = first_art + arts in
    let table = Array.init m (fun _ -> Array.make (cols + 1) F.zero) in
    let basis = Array.make m 0 and slack = ref vars and art = ref first_art in
    Array.iteri
      (fun i r ->
        let t = table.(i) in
        List.iter
          (fun (j, c) ->
            if j < 0 || j >= vars then invalid_arg "Lp.maximize: variable";
            t.(j) <- F.add t.(j) (F.of_rat c))
          r.coeffs;
        t.(cols) <- F.of_rat r.rhs;
        (match r.rel with
        | Le ->
            t.(!slack) <- F.one;
            basis.(i) <- !slack
        | Ge ->
            t.(!slack) <- F.sub F.zero F.one;
            t.(!art) <- F.one;
            basis.(i) <- !art
        | Eq ->
            t.(!art) <- F.one;
            basis.(i) <- !art);
        if r.rel <> Eq then incr slack;
        if r.rel <> Le then incr art)
      rows;
    let t =
      {
        rows = table;
        basis;
        obj = Array.make (cols + 1) F.zero;
        cols;
        init = Array.copy basis;
        pivots = 0;
        harris = false;
      }
    in
    (t, first_art)

  let phase1 ?limit ?(slack = F.zero) t ~bland ~first_art =
    price t (fun j -> if j >= first_art then F.sub F.zero F.one else F.zero);
    run ?limit t ~bland ~allowed:t.cols = `Optimal
    && F.compare (F.add (rhs t t.obj) slack) F.zero >= 0

  let primal t = Array.for_all (fun r -> F.sign (rhs t r) >= 0) t.rows

  let expel t ~first_art =
    Array.iteri
      (fun i r ->
        if t.basis.(i) >= first_art then
          let rec find j =
            if j < first_art then
              if zero r.(j) then find (j + 1) else pivot t i j
          in
          find 0)
      t.rows

  let phase2 ?limit t ~bland ~first_art cost =
    expel t ~first_art;
    price t (fun j -> if j < first_art then cost j else F.zero);
    run ?limit t ~bland ~allowed:first_art

  let restore t b =
    Array.iter
      (fun r ->
        let v = ref F.zero in
        Array.iteri (fun k c -> v := F.add !v (F.mul r.(t.init.(k)) c)) b;
        r.(t.cols) <- !v)
      t.rows;
    let v = ref F.zero in
    Array.iteri (fun k c -> v := F.add !v (F.mul t.obj.(t.init.(k)) c)) b;
    t.obj.(t.cols) <- !v

  let dual ?(limit = max_int) ?(slack = F.zero) t ~allowed =
    let rec loop () =
      let r = ref (-1) in
      Array.iteri
        (fun i row ->
          if
            F.compare (F.add (rhs t row) slack) F.zero < 0
            && (!r < 0 || F.compare (rhs t row) (rhs t t.rows.(!r)) < 0)
          then r := i)
        t.rows;
      if !r < 0 then `Optimal
      else if t.pivots >= limit then `Stopped
      else
        let row = t.rows.(!r) and s = ref (-1) in
        for j = 0 to allowed - 1 do
          if F.sign row.(j) < 0 then
            let q = F.div t.obj.(j) (F.sub F.zero row.(j)) in
            if
              !s < 0
              || F.compare q (F.div t.obj.(!s) (F.sub F.zero row.(!s))) < 0
            then s := j
        done;
        if !s < 0 then `Infeasible
        else (
          pivot t !r !s;
          loop ())
    in
    loop ()

  let install t target =
    let wanted = Hashtbl.create 64 in
    List.iter (fun j -> Hashtbl.replace wanted j ()) target;
    List.iter
      (fun j ->
        if not (Array.exists (( = ) j) t.basis) then
          let rec find i =
            if i < Array.length t.rows then
              if
                (not (Hashtbl.mem wanted t.basis.(i)))
                && not (zero t.rows.(i).(j))
              then pivot t i j
              else find (i + 1)
          in
          find 0)
      target
end

module A = Simplex (Approx)
module E = Simplex (Exact)

let flip = function Le -> Ge | Ge -> Le | Eq -> Eq

let normalize rows =
  List.map
    (fun r ->
      if Rat.sign r.rhs >= 0 then r
      else
        {
          coeffs = List.map (fun (j, c) -> (j, Rat.neg c)) r.coeffs;
          rel = flip r.rel;
          rhs = Rat.neg r.rhs;
        })
    rows
  |> Array.of_list

let float_basis ~bland ~vars c rows =
  let t, first_art = A.setup ~vars rows in
  let limit = 20 * (Array.length rows + t.cols) in
  if not (A.phase1 ~limit t ~bland ~first_art) then None
  else
    match A.phase2 ~limit t ~bland ~first_art (fun j -> Rat.to_float c.(j)) with
    | `Optimal -> Some (Array.to_list t.basis)
    | `Unbounded | `Stopped -> None

let finish ~bland ~vars ~first_art c t =
  match E.phase2 t ~bland ~first_art (fun j -> c.(j)) with
  | `Unbounded | `Stopped -> Unbounded
  | `Optimal ->
      let x = Array.make vars Rat.zero in
      Array.iteri
        (fun i b -> if b < vars then x.(b) <- E.rhs t t.rows.(i))
        t.basis;
      Optimal { value = E.rhs t t.obj; x; pivots = t.pivots }

let maximize ?(bland = false) ?(warm = true) ~vars objective rows =
  let rows = normalize rows in
  let t, first_art = E.setup ~vars rows in
  let c = Array.make first_art Rat.zero in
  List.iter
    (fun (j, v) ->
      if j < 0 || j >= vars then invalid_arg "Lp.maximize: variable";
      c.(j) <- Rat.add c.(j) v)
    objective;
  let t =
    match if warm then float_basis ~bland ~vars c rows else None with
    | Some basis ->
        E.install t basis;
        if E.primal t then t else fst (E.setup ~vars rows)
    | None -> t
  in
  if E.phase1 t ~bland ~first_art then finish ~bland ~vars ~first_art c t
  else Infeasible

let float_problem name ~vars objective rows =
  let rows = normalize rows in
  let t, first_art = A.setup ~vars rows in
  let c = Array.make first_art 0. in
  List.iter
    (fun (j, v) ->
      if j < 0 || j >= vars then invalid_arg (name ^ ": variable");
      c.(j) <- c.(j) +. Rat.to_float v)
    objective;
  (t, first_art, c)

let primal t ~vars shift =
  let x = Array.map Float.neg shift in
  Array.iteri
    (fun i b -> if b < vars then x.(b) <- x.(b) +. A.rhs t t.rows.(i))
    t.basis;
  x

let maximize_float ?(bland = false) ?limit ~vars objective rows =
  let t, first_art, c =
    float_problem "Lp.maximize_float" ~vars objective rows
  in
  if not (A.phase1 ?limit t ~bland ~first_art) then None
  else
    match A.phase2 ?limit t ~bland ~first_art (fun j -> c.(j)) with
    | `Unbounded | `Stopped -> None
    | `Optimal -> Some (A.rhs t t.obj, primal t ~vars (Array.make vars 0.))

let bound_float ?limit ~vars objective rows =
  let d =
    Array.init vars (fun j ->
        Rat.of_ints (1000 + ((j + 1) * 7919 mod 1000)) 1_000_000_000)
  in
  let moved =
    List.map
      (fun r ->
        let rhs =
          List.fold_left
            (fun acc (j, c) -> Rat.add acc (Rat.mul c d.(j)))
            r.rhs r.coeffs
        in
        ({ r with rhs }, if Rat.sign rhs < 0 then Rat.neg r.rhs else r.rhs))
      rows
  in
  let original =
    Array.of_list (List.map (fun (_, b) -> Rat.to_float b) moved)
  in
  let t, first_art, c =
    float_problem "Lp.bound_float" ~vars objective (List.map fst moved)
  in
  t.harris <- true;
  if not (A.phase1 ?limit ~slack:1e-6 t ~bland:false ~first_art) then None
  else
    match A.phase2 ?limit t ~bland:false ~first_art (fun j -> c.(j)) with
    | `Unbounded | `Stopped -> None
    | `Optimal -> (
        let near = primal t ~vars (Array.map Rat.to_float d) in
        A.restore t original;
        match A.dual ?limit ~slack:1e-7 t ~allowed:first_art with
        | `Optimal -> Some (A.rhs t t.obj, primal t ~vars (Array.make vars 0.))
        | `Infeasible | `Stopped -> Some (A.rhs t t.obj, near))
