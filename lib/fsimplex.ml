type problem = {
  m : int;
  first_art : int;
  columns : (int * float) array array;
  b : float array;
  cost : float array;
  start : int array;
}

type status = Optimal | Infeasible | Unbounded | Stopped

type result = {
  status : status;
  basis : int array;
  value : float;
  x : float array;
  pivots : int;
  refactors : int;
}

let dual_tol = 1e-9
let pivot_tol = 1e-9
let harris = 1e-9
let eta_limit = 128

type eta = { r : int; ar : float; rest : (int * float) array }

type t = {
  p : problem;
  basis : int array;
  pos : int array;
  mutable lu : Flu.t;
  etas : eta Dynarray.t;
  mutable xb : float array;
  d : float array;
  w : float array;
  rows : (int * float) array array;
  arow : float array;
  touched : int Dynarray.t;
  mutable fill : int;
  mutable budget : int;
  mutable pivots : int;
  mutable refactors : int;
}

let ftran t v =
  let v = Flu.solve t.lu v in
  Dynarray.iter
    (fun e ->
      let vr = v.(e.r) /. e.ar in
      v.(e.r) <- vr;
      if vr <> 0. then
        Array.iter (fun (i, a) -> v.(i) <- v.(i) -. (a *. vr)) e.rest)
    t.etas;
  v

let btran t c =
  let c = Array.copy c in
  for k = Dynarray.length t.etas - 1 downto 0 do
    let e = Dynarray.get t.etas k in
    let s =
      Array.fold_left (fun acc (i, a) -> acc -. (c.(i) *. a)) c.(e.r) e.rest
    in
    c.(e.r) <- s /. e.ar
  done;
  Flu.solve_t t.lu c

let dense t col =
  let v = Array.make t.p.m 0. in
  Array.iter (fun (i, a) -> v.(i) <- a) col;
  v

let dot y col = Array.fold_left (fun acc (i, a) -> acc +. (y.(i) *. a)) 0. col

let factor p basis =
  Flu.factor p.m (Array.map (fun j -> Array.to_list p.columns.(j)) basis)

let refresh t cost =
  t.xb <- ftran t t.p.b;
  let y = btran t (Array.map cost t.basis) in
  for j = 0 to t.p.first_art - 1 do
    t.d.(j) <- (if t.pos.(j) >= 0 then 0. else cost j -. dot y t.p.columns.(j))
  done

let entering t rejected =
  let best = ref (-1) and score = ref 0. in
  for j = 0 to t.p.first_art - 1 do
    let dj = t.d.(j) in
    if t.pos.(j) < 0 && dj > dual_tol && not (Hashtbl.mem rejected j) then
      let s = dj *. dj /. t.w.(j) in
      if s > !score then (
        best := j;
        score := s)
  done;
  !best

let leaving t alpha ~phase2 =
  let blocks i = phase2 && t.basis.(i) >= t.p.first_art in
  let bound = ref infinity in
  Array.iteri
    (fun i a ->
      if blocks i then (
        if Float.abs a > pivot_tol then
          bound :=
            Float.min !bound ((Float.abs t.xb.(i) +. harris) /. Float.abs a))
      else if a > pivot_tol then
        bound := Float.min !bound ((t.xb.(i) +. harris) /. a))
    alpha;
  if !bound = infinity then None
  else
    let best = ref (-1) in
    Array.iteri
      (fun i a ->
        let ratio =
          if blocks i then if Float.abs a > pivot_tol then Some 0. else None
          else if a > pivot_tol then Some (t.xb.(i) /. a)
          else None
        in
        match ratio with
        | Some q when q <= !bound ->
            if !best < 0 || Float.abs a > Float.abs alpha.(!best) then best := i
        | _ -> ())
      alpha;
    let r = !best in
    Some (r, if blocks r then 0. else Float.max 0. (t.xb.(r) /. alpha.(r)))

let swap t r j =
  let l = t.basis.(r) in
  t.pos.(l) <- -1;
  t.basis.(r) <- j;
  t.pos.(j) <- r;
  l

let rec iterate t ~limit ~phase2 cost rejected =
  if t.pivots >= limit then Stopped
  else
    let q = entering t rejected in
    if q < 0 then Optimal
    else
      let alpha = ftran t (dense t t.p.columns.(q)) in
      match leaving t alpha ~phase2 with
      | None -> Unbounded
      | Some (r, theta) -> (
          let arq = alpha.(r) in
          let l = swap t r q in
          if Dynarray.length t.etas < eta_limit && t.fill < t.budget then (
            pivot t ~r ~q ~l ~alpha ~theta;
            let rest = ref [] in
            Array.iteri
              (fun i a ->
                if i <> r && Float.abs a > Flu.drop then rest := (i, a) :: !rest)
              alpha;
            let rest = Array.of_list !rest in
            t.fill <- t.fill + Array.length rest;
            Dynarray.add_last t.etas { r; ar = arq; rest };
            iterate t ~limit ~phase2 cost rejected)
          else
            match factor t.p t.basis with
            | None ->
                ignore (swap t r l);
                Hashtbl.replace rejected q ();
                iterate t ~limit ~phase2 cost rejected
            | Some lu ->
                pivot t ~r ~q ~l ~alpha ~theta;
                t.lu <- lu;
                Dynarray.clear t.etas;
                t.fill <- 0;
                t.budget <- 3 * (Flu.nonzeros lu + t.p.m);
                t.refactors <- t.refactors + 1;
                Hashtbl.reset rejected;
                refresh t cost;
                iterate t ~limit ~phase2 cost rejected)

and pivot t ~r ~q ~l ~alpha ~theta =
  let arq = alpha.(r) in
  let e = Array.make t.p.m 0. in
  e.(r) <- 1.;
  let rho = btran t e in
  let dq = t.d.(q) and wq = t.w.(q) in
  let step = dq /. arq in
  Array.iteri
    (fun i ri ->
      if Float.abs ri > Flu.drop then
        Array.iter
          (fun (j, a) ->
            if t.arow.(j) = 0. then Dynarray.add_last t.touched j;
            t.arow.(j) <- t.arow.(j) +. (ri *. a))
          t.rows.(i))
    rho;
  Dynarray.iter
    (fun j ->
      let arj = t.arow.(j) in
      t.arow.(j) <- 0.;
      if t.pos.(j) < 0 && j <> l && arj <> 0. then (
        t.d.(j) <- t.d.(j) -. (step *. arj);
        let ratio = arj /. arq in
        t.w.(j) <- Float.max t.w.(j) (ratio *. ratio *. wq)))
    t.touched;
  Dynarray.clear t.touched;
  t.d.(q) <- 0.;
  if l < t.p.first_art then (
    t.d.(l) <- -.step;
    t.w.(l) <- Float.max (wq /. (arq *. arq)) 1.);
  Array.iteri (fun i a -> t.xb.(i) <- t.xb.(i) -. (theta *. a)) alpha;
  t.xb.(r) <- theta;
  t.pivots <- t.pivots + 1

let result t status cost =
  let x = Array.make (Array.length t.p.columns) 0. in
  Array.iteri (fun i j -> x.(j) <- t.xb.(i)) t.basis;
  let value = ref 0. in
  Array.iteri (fun i j -> value := !value +. (cost j *. t.xb.(i))) t.basis;
  {
    status;
    basis = Array.copy t.basis;
    value = !value;
    x;
    pivots = t.pivots;
    refactors = t.refactors;
  }

let transpose p =
  let rows = Array.make p.m [] in
  for j = p.first_art - 1 downto 0 do
    Array.iter (fun (i, a) -> rows.(i) <- (j, a) :: rows.(i)) p.columns.(j)
  done;
  Array.map Array.of_list rows

let solve ?(limit = max_int) p =
  let n = Array.length p.columns in
  if Array.length p.start <> p.m || Array.length p.b <> p.m then
    invalid_arg "Fsimplex.solve: start";
  let lu =
    match factor p p.start with
    | Some lu -> lu
    | None -> invalid_arg "Fsimplex.solve: singular start"
  in
  let pos = Array.make n (-1) in
  Array.iteri (fun i j -> pos.(j) <- i) p.start;
  let t =
    {
      p;
      basis = Array.copy p.start;
      pos;
      lu;
      etas = Dynarray.create ();
      xb = [||];
      d = Array.make p.first_art 0.;
      w = Array.make p.first_art 1.;
      rows = transpose p;
      arow = Array.make p.first_art 0.;
      touched = Dynarray.create ();
      fill = 0;
      budget = 3 * (Flu.nonzeros lu + p.m);
      pivots = 0;
      refactors = 0;
    }
  in
  let art j = if j >= p.first_art then -1. else 0. in
  let real j = if j < p.first_art then p.cost.(j) else 0. in
  refresh t art;
  let scale = Array.fold_left (fun a v -> Float.max a (Float.abs v)) 1. p.b in
  match iterate t ~limit ~phase2:false art (Hashtbl.create 8) with
  | Stopped | Unbounded | Infeasible -> result t Stopped real
  | Optimal -> (
      let infeasibility =
        Array.fold_left
          (fun acc i ->
            if t.basis.(i) >= p.first_art then acc +. t.xb.(i) else acc)
          0. (Array.init p.m Fun.id)
      in
      Array.fill t.w 0 p.first_art 1.;
      refresh t real;
      match iterate t ~limit ~phase2:true real (Hashtbl.create 8) with
      | Stopped -> result t Stopped real
      | _ when infeasibility > 1e-7 *. scale -> result t Infeasible real
      | status -> result t status real)
