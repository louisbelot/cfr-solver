open Multi_tree

type mode = Threshold of float | Penalty of float

type shape = {
  infosets : int array;
  base : int array;
  up : int array;
  first : int array;
  kids : int array;
}

type trigger = {
  shape : shape;
  dev_at : int array;
  dev_pair : int array;
  dev_u : float array;
  obey_z : int array;
  obey_u : float array;
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
  let first = Array.make (Array.length kids + 1) 0 in
  Array.iteri (fun a k -> first.(a + 1) <- first.(a) + Array.length k) kids;
  ( { infosets; base; up; first; kids = Array.concat (Array.to_list kids) },
    local )

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
        let d = Array.of_list dev.(t) and o = Array.of_list obey.(t) in
        {
          shape = fst (shape t);
          dev_at = Array.map (fun (l, _, _) -> l) d;
          dev_pair = Array.map (fun (_, p, _) -> p) d;
          dev_u = Array.map (fun (_, _, u) -> u) d;
          obey_z = Array.map fst o;
          obey_u = Array.map snd o;
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
let uniform g = Xi.play (Xi.learner g.xi)

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

let width tr = tr.shape.base.(Array.length tr.shape.infosets)

let obeyed tr x =
  let acc = ref 0. in
  for i = 0 to Array.length tr.obey_z - 1 do
    acc := !acc +. (tr.obey_u.(i) *. x.(tr.obey_z.(i)))
  done;
  !acc

let deviation tr x gl =
  Array.fill gl 0 (width tr) 0.;
  for i = 0 to Array.length tr.dev_at - 1 do
    let l = tr.dev_at.(i) in
    gl.(l) <- gl.(l) +. (tr.dev_u.(i) *. x.(tr.dev_pair.(i)))
  done

let backward sh gl v visit =
  for m = Array.length sh.infosets - 1 downto 0 do
    for a = sh.base.(m) to sh.base.(m + 1) - 1 do
      let acc = ref gl.(a) in
      for k = sh.first.(a) to sh.first.(a + 1) - 1 do
        acc := !acc +. v.(sh.kids.(k))
      done;
      gl.(a) <- !acc
    done;
    visit m sh.base.(m) sh.base.(m + 1)
  done;
  let total = ref 0. in
  Array.iteri (fun m u -> if u < 0 then total := !total +. v.(m)) sh.up;
  !total

let buffers g =
  let most f = Array.fold_left (fun acc tr -> max acc (f tr)) 0 g.triggers in
  ( Array.make (most width) 0.,
    Array.make (most (fun tr -> Array.length tr.shape.infosets)) 0. )

let gain_of tr x (gl, v) =
  deviation tr x gl;
  backward tr.shape gl v (fun m lo hi ->
      let b = ref neg_infinity in
      for a = lo to hi - 1 do
        if gl.(a) > !b then b := gl.(a)
      done;
      v.(m) <- !b)
  -. obeyed tr x

let gains g x =
  let bufs = buffers g in
  Array.map (fun tr -> gain_of tr x bufs) g.triggers

let gain g x = Array.fold_left Float.max neg_infinity (gains g x)

let gradient g mode lam ys coef d =
  Array.fill d 0 (Array.length d) 0.;
  let w, scale =
    match mode with Threshold _ -> (lam.(0), 1.) | Penalty k -> (1., k)
  in
  Array.iter (fun (z, wz) -> d.(z) <- d.(z) +. (w *. wz)) g.welfare;
  Array.iteri
    (fun o tr ->
      let l = scale *. lam.(o + 1) in
      if l > 0. then begin
        for i = 0 to Array.length tr.obey_z - 1 do
          let z = tr.obey_z.(i) in
          d.(z) <- d.(z) +. (l *. tr.obey_u.(i))
        done;
        let y = ys.(o) and c = coef.(o) in
        for i = 0 to Array.length tr.dev_at - 1 do
          let p = tr.dev_pair.(i) in
          d.(p) <- d.(p) -. (scale *. tr.dev_u.(i) *. (c *. y.(tr.dev_at.(i))))
        done
      end)
    g.triggers

let constant mode lam =
  match mode with Threshold tau -> -.lam.(0) *. tau | Penalty _ -> 0.

let[@inline] pos x = if x > 0. then x else 0.

let regret_match r m y lo hi =
  let total = ref 0. in
  for a = lo to hi - 1 do
    total := !total +. pos (r.(a) +. m.(a))
  done;
  for a = lo to hi - 1 do
    y.(a) <-
      (if !total > 0. then pos (r.(a) +. m.(a)) /. !total
       else 1. /. float_of_int (hi - lo))
  done

let realize sh y x =
  for m = 0 to Array.length sh.infosets - 1 do
    let p = if sh.up.(m) < 0 then 1. else x.(sh.up.(m)) in
    for a = sh.base.(m) to sh.base.(m + 1) - 1 do
      x.(a) <- p *. y.(a)
    done
  done

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
  gl : float array;
  v : float array;
  grad : float array;
  coef : float array;
  mutable t : int;
}

let refresh s =
  regret_match s.top s.top_last s.lam 0 (Array.length s.lam);
  Array.iteri
    (fun o tr ->
      let base = tr.shape.base in
      for m = 0 to Array.length tr.shape.infosets - 1 do
        regret_match s.regret.(o) s.last.(o) s.y.(o) base.(m) base.(m + 1)
      done;
      realize tr.shape s.y.(o) s.plan.(o))
    s.game.triggers

let solver ?(predictive = false) game mode =
  let n = Array.length game.triggers in
  let per f = Array.map (fun tr -> Array.make (width tr) f) game.triggers in
  let gl, v = buffers game in
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
      gl;
      v;
      grad = Array.make (Xi.size game.xi) 0.;
      coef = Array.make n 0.;
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
  let gl = s.gl and vs = s.v in
  Array.iteri
    (fun o tr ->
      let r = s.regret.(o) and m = s.last.(o) and y = s.y.(o) in
      deviation tr x gl;
      let v =
        backward tr.shape gl vs (fun k lo hi ->
            let ev = ref 0. in
            for a = lo to hi - 1 do
              ev := !ev +. (y.(a) *. gl.(a))
            done;
            for a = lo to hi - 1 do
              let d = gl.(a) -. !ev in
              if s.predictive then m.(a) <- d;
              r.(a) <- pos (r.(a) +. d)
            done;
            vs.(k) <- !ev)
      in
      value.(o + 1) <- scale *. (v -. obeyed tr x))
    g.triggers;
  let ev = ref 0. in
  Array.iteri (fun o v -> ev := !ev +. (s.lam.(o) *. v)) value;
  Array.iteri
    (fun o v ->
      let d = v -. !ev in
      if s.predictive then s.top_last.(o) <- d;
      s.top.(o) <- pos (s.top.(o) +. d))
    value;
  refresh s;
  Array.iteri (fun o l -> s.lbar.(o) <- s.lbar.(o) +. (w *. l)) s.lam;
  Array.iteri
    (fun o p ->
      let l = s.lam.(o + 1) in
      let yb = s.ybar.(o) in
      for a = 0 to Array.length p - 1 do
        yb.(a) <- yb.(a) +. (w *. l *. p.(a))
      done;
      s.coef.(o) <- l)
    s.plan;
  gradient g s.mode s.lam s.plan s.coef s.grad;
  Xi.update s.med s.grad

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
  let d = Array.make (Xi.size s.game.xi) 0. in
  gradient s.game s.mode lam ytil (Array.make (Array.length ytil) 1.) d;
  let best, _ = Xi.best s.game.xi d in
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
