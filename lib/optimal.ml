open Multi_tree

type mode = Threshold of float | Penalty of float

type shape = {
  infosets : int array;
  base : int array;
  up : int array;
  kids : int array array;
}

type trigger = {
  shape : shape;
  dev : (int * int * float) array;
  obey : (int * float) array;
}

type t = {
  xi : Xi.t;
  welfare : (int * float) array;
  payoffs : (int * float array) array;
  triggers : trigger array;
  lo : float;
  hi : float;
}

let shape_of (tp : Treeplex.t) roots =
  let off = tp.tree.offset in
  let infosets =
    Array.of_list
      (List.concat_map (fun k -> Array.to_list tp.subtree.(k)) roots)
  in
  let pos = Hashtbl.create 16 in
  Array.iteri (fun m j -> Hashtbl.replace pos j m) infosets;
  let base = Array.make (Array.length infosets + 1) 0 in
  Array.iteri
    (fun m j -> base.(m + 1) <- base.(m) + off.(j + 1) - off.(j))
    infosets;
  let local s =
    let j = Treeplex.infoset tp s in
    base.(Hashtbl.find pos j) + s - off.(j)
  in
  let up =
    Array.map
      (fun j -> if List.mem j roots then -1 else local tp.parent.(j))
      infosets
  in
  let kids = Array.make base.(Array.length infosets) [||] in
  Array.iter
    (fun j ->
      for s = off.(j) to off.(j + 1) - 1 do
        kids.(local s) <- Array.map (Hashtbl.find pos) tp.below.(s)
      done)
    infosets;
  ({ infosets; base; up; kids }, local)

let create ?(concept = Efce_lp.Efce) plan =
  let tp = Efce_lp.treeplex plan in
  let tree = tp.tree in
  let off = tree.offset in
  let get i s o =
    match if i = 0 then Efce_lp.index plan s o else Efce_lp.index plan o s with
    | Some x -> x
    | None -> invalid_arg "Optimal.create: irrelevant pair"
  in
  let infosets = Array.length tree.keys in
  let roots pl =
    List.filter
      (fun k -> tree.owner.(k) = pl && tp.parent.(k) < 0)
      (Array.to_list tp.order)
  in
  let count, region =
    match concept with
    | Efce -> (slots tree, fun t -> [ Treeplex.infoset tp t ])
    | Efcce -> (infosets, fun t -> [ t ])
    | Nfcce -> (2, roots)
  in
  let memo = Hashtbl.create 64 in
  let shape t =
    let r = region t in
    match Hashtbl.find_opt memo r with
    | Some sh -> sh
    | None ->
        let sh = shape_of tp r in
        Hashtbl.add memo r sh;
        sh
  in
  let local t s = snd (shape t) s in
  let dev = Array.make count [] and obey = Array.make count [] in
  let add t entry z =
    dev.(t) <- entry :: dev.(t);
    obey.(t) <- z @ obey.(t)
  in
  let welfare = ref [] and payoffs = ref [] in
  List.iter
    (fun (s0, s1, u) ->
      let z = get 0 s0 s1 in
      welfare := (z, u.(0) +. u.(1)) :: !welfare;
      payoffs := (z, u) :: !payoffs;
      List.iter
        (fun (i, s, o) ->
          let rec climb a =
            if a >= 0 then begin
              let k = Treeplex.infoset tp a in
              (match concept with
              | Efce ->
                  for b = off.(k) to off.(k + 1) - 1 do
                    add b
                      (local b s, get i b o, u.(i))
                      (if b = a then [ (z, u.(i)) ] else [])
                  done
              | Efcce ->
                  add k (local k s, get i tp.parent.(k) o, u.(i)) [ (z, u.(i)) ]
              | Nfcce ->
                  if tp.parent.(k) < 0 then
                    add i (local i s, get i (-1) o, u.(i)) [ (z, u.(i)) ]);
              climb tp.parent.(k)
            end
          in
          climb s)
        [ (0, s0, s1); (1, s1, s0) ])
    (Efce_lp.leaves plan);
  let triggers =
    Array.init count (fun t ->
        {
          shape = fst (shape t);
          dev = Array.of_list dev.(t);
          obey = Array.of_list obey.(t);
        })
  in
  let ws = List.map snd !welfare in
  {
    xi = Xi.build plan;
    welfare = Array.of_list !welfare;
    payoffs = Array.of_list !payoffs;
    triggers;
    lo = List.fold_left Float.min infinity ws;
    hi = List.fold_left Float.max neg_infinity ws;
  }

let range g = (g.lo, g.hi)
let size g = Xi.size g.xi
let triggers g = Array.length g.triggers

let welfare g x =
  Array.fold_left (fun acc (z, w) -> acc +. (w *. x.(z))) 0. g.welfare

let values g x =
  let v = [| 0.; 0. |] in
  Array.iter
    (fun (z, u) ->
      v.(0) <- v.(0) +. (u.(0) *. x.(z));
      v.(1) <- v.(1) +. (u.(1) *. x.(z)))
    g.payoffs;
  v

let obeyed tr x =
  Array.fold_left (fun acc (z, u) -> acc +. (u *. x.(z))) 0. tr.obey

let deviation tr x =
  let gl = Array.make tr.shape.base.(Array.length tr.shape.infosets) 0. in
  Array.iter (fun (l, pair, u) -> gl.(l) <- gl.(l) +. (u *. x.(pair))) tr.dev;
  gl

let backward sh gl visit =
  let v = Array.make (Array.length sh.infosets) 0. in
  for m = Array.length sh.infosets - 1 downto 0 do
    let lo = sh.base.(m) and hi = sh.base.(m + 1) in
    let q =
      Array.init (hi - lo) (fun b ->
          Array.fold_left
            (fun acc m' -> acc +. v.(m'))
            gl.(lo + b)
            sh.kids.(lo + b))
    in
    v.(m) <- visit lo q
  done;
  let total = ref 0. in
  Array.iteri (fun m u -> if u < 0 then total := !total +. v.(m)) sh.up;
  !total

let gain_of tr x =
  backward tr.shape (deviation tr x) (fun _ q ->
      Array.fold_left Float.max neg_infinity q)
  -. obeyed tr x

let gains g x = Array.map (fun tr -> gain_of tr x) g.triggers
let gain g x = Array.fold_left Float.max neg_infinity (gains g x)

let gradient g mode lam ytil =
  let d = Array.make (Xi.size g.xi) 0. in
  let w, scale =
    match mode with Threshold _ -> (lam.(0), 1.) | Penalty k -> (1., k)
  in
  Array.iter (fun (z, wz) -> d.(z) <- d.(z) +. (w *. wz)) g.welfare;
  Array.iteri
    (fun o tr ->
      let l = scale *. lam.(o + 1) in
      if l > 0. then begin
        Array.iter (fun (z, u) -> d.(z) <- d.(z) +. (l *. u)) tr.obey;
        let y = ytil.(o) in
        Array.iter
          (fun (loc, pair, u) ->
            d.(pair) <- d.(pair) -. (scale *. u *. y.(loc)))
          tr.dev
      end)
    g.triggers;
  d

let constant mode lam =
  match mode with Threshold tau -> -.lam.(0) *. tau | Penalty _ -> 0.

let regret_match r m y lo hi =
  let w a = Float.max 0. (r.(a) +. m.(a)) in
  let total = ref 0. in
  for a = lo to hi - 1 do
    total := !total +. w a
  done;
  for a = lo to hi - 1 do
    y.(a) <-
      (if !total > 0. then w a /. !total else 1. /. float_of_int (hi - lo))
  done

let realize sh y x =
  Array.iteri
    (fun m _ ->
      let p = if sh.up.(m) < 0 then 1. else x.(sh.up.(m)) in
      for a = sh.base.(m) to sh.base.(m + 1) - 1 do
        x.(a) <- p *. y.(a)
      done)
    sh.infosets

type bounds = { lower : float; upper : float; welfare : float; gain : float }

type solver = {
  game : t;
  mode : mode;
  predictive : bool;
  med : Xi.learner;
  top : float array;
  top_last : float array;
  lam : float array;
  regret : float array array;
  last : float array array;
  y : float array array;
  plan : float array array;
  xbar : float array;
  lbar : float array;
  ybar : float array array;
  mutable t : int;
}

let width tr = tr.shape.base.(Array.length tr.shape.infosets)

let refresh s =
  regret_match s.top s.top_last s.lam 0 (Array.length s.lam);
  Array.iteri
    (fun o tr ->
      Array.iteri
        (fun m _ ->
          regret_match s.regret.(o) s.last.(o) s.y.(o) tr.shape.base.(m)
            tr.shape.base.(m + 1))
        tr.shape.infosets;
      realize tr.shape s.y.(o) s.plan.(o))
    s.game.triggers

let solver ?(predictive = false) game mode =
  let n = Array.length game.triggers in
  let per f = Array.map (fun tr -> Array.make (width tr) f) game.triggers in
  let s =
    {
      game;
      mode;
      predictive;
      med = Xi.learner ~predictive game.xi;
      top = Array.make (n + 1) 0.;
      top_last = Array.make (n + 1) 0.;
      lam = Array.make (n + 1) 0.;
      regret = per 0.;
      last = per 0.;
      y = per 0.;
      plan = per 0.;
      xbar = Array.make (Xi.size game.xi) 0.;
      lbar = Array.make (n + 1) 0.;
      ybar = per 0.;
      t = 0;
    }
  in
  refresh s;
  s

let iterate s =
  s.t <- s.t + 1;
  let w = float_of_int s.t in
  let g = s.game in
  let x = Xi.play s.med in
  Array.iteri (fun i v -> s.xbar.(i) <- s.xbar.(i) +. (w *. v)) x;
  let value = Array.make (Array.length s.lam) 0. in
  value.(0) <-
    (match s.mode with Threshold tau -> tau -. welfare g x | Penalty _ -> 0.);
  let scale = match s.mode with Threshold _ -> 1. | Penalty k -> k in
  Array.iteri
    (fun o tr ->
      let r = s.regret.(o) and m = s.last.(o) and y = s.y.(o) in
      let v =
        backward tr.shape (deviation tr x) (fun lo q ->
            let ev = ref 0. in
            Array.iteri (fun b qb -> ev := !ev +. (y.(lo + b) *. qb)) q;
            Array.iteri
              (fun b qb ->
                let d = qb -. !ev in
                if s.predictive then m.(lo + b) <- d;
                r.(lo + b) <- Float.max 0. (r.(lo + b) +. d))
              q;
            !ev)
      in
      value.(o + 1) <- scale *. (v -. obeyed tr x))
    g.triggers;
  let ev = ref 0. in
  Array.iteri (fun o v -> ev := !ev +. (s.lam.(o) *. v)) value;
  Array.iteri
    (fun o v ->
      let d = v -. !ev in
      if s.predictive then s.top_last.(o) <- d;
      s.top.(o) <- Float.max 0. (s.top.(o) +. d))
    value;
  refresh s;
  Array.iteri (fun o l -> s.lbar.(o) <- s.lbar.(o) +. (w *. l)) s.lam;
  let ytil =
    Array.mapi
      (fun o p ->
        let l = s.lam.(o + 1) in
        let yb = s.ybar.(o) in
        Array.iteri (fun a v -> yb.(a) <- yb.(a) +. (w *. l *. v)) p;
        Array.map (fun v -> l *. v) p)
      s.plan
  in
  Xi.update s.med (gradient g s.mode s.lam ytil)

let run s n =
  for _ = 1 to n do
    iterate s
  done

let iterations s = s.t
let total s = float_of_int (s.t * (s.t + 1) / 2)
let average s = Array.map (fun v -> v /. total s) s.xbar

let bounds s =
  let x = average s in
  let z = total s in
  let lam = Array.map (fun v -> v /. z) s.lbar in
  let ytil = Array.map (Array.map (fun v -> v /. z)) s.ybar in
  let best, _ = Xi.best s.game.xi (gradient s.game s.mode lam ytil) in
  let w = welfare s.game x and worst = gain s.game x in
  let lower =
    match s.mode with
    | Threshold tau -> Float.min (w -. tau) (-.worst)
    | Penalty k -> w -. (k *. Float.max 0. worst)
  in
  { lower; upper = best +. constant s.mode lam; welfare = w; gain = worst }

type step = {
  tau : float;
  feasible : bool option;
  iterations : int;
  bounds : bounds;
  xi : float array;
}

let decide ?(eps = 1e-3) ?(every = 10) ?(limit = 100_000) ?predictive g tau =
  let s = solver ?predictive g (Threshold tau) in
  let rec go () =
    run s every;
    let b = bounds s in
    let verdict =
      if b.lower >= -.eps then Some (Some true)
      else if b.upper < 0. then Some (Some false)
      else if s.t >= limit then Some None
      else None
    in
    match verdict with
    | Some feasible ->
        { tau; feasible; iterations = s.t; bounds = b; xi = average s }
    | None -> go ()
  in
  go ()

let bisect ?(eps = 1e-3) ?every ?limit ?predictive g =
  let rec go lo hi best steps =
    if hi -. lo <= eps then (best, List.rev steps)
    else
      let st = decide ~eps ?every ?limit ?predictive g ((lo +. hi) /. 2.) in
      match st.feasible with
      | Some true -> go st.tau hi (Some st) (st :: steps)
      | Some false | None -> go lo st.tau best (st :: steps)
  in
  let best, steps = go g.lo g.hi None [] in
  match best with
  | Some st -> (st, steps)
  | None ->
      let st = decide ~eps ?every ?limit ?predictive g g.lo in
      (st, steps @ [ st ])
