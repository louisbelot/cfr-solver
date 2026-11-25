type point = {
  iteration : int;
  touched : int;
  average : float;
  current : float option;
}

type row = {
  iteration : int;
  average : float array;
  current : float array;
  regret : float array;
}

type frame = {
  index : int;
  at : int;
  avg : float array;
  cur : float array;
  reg : float array;
}

type engine = {
  infosets : Tree.infoset array;
  step : unit -> unit;
  count : unit -> int;
  nodes : unit -> int;
  avg : unit -> Strategy.t;
  cur : unit -> Strategy.t;
  reg : unit -> (string * float array) list;
}

type t = {
  game : (module Game.S);
  average : unit -> Strategy.t;
  infosets : Tree.infoset array;
  offset : int array;
  lookup : (string, int) Hashtbl.t;
  step : unit -> unit;
  iterations : unit -> int;
  touched : unit -> int;
  snapshot : unit -> float array * float array * float array;
  exploit : unit -> float * float option;
  budget : int;
  growth : float;
  mutable next : int;
  mutable count : int;
  mutable stride : int;
  mutable points : point list;
  mutable frames : frame list;
}

let full algorithm (module G : Game.S) : engine =
  let module C = Cfr.Make (G) in
  let c = C.create ~algorithm () in
  {
    infosets = C.infosets c;
    step = (fun () -> C.iterate c);
    count = (fun () -> C.iterations c);
    nodes = (fun () -> C.touched c);
    avg = (fun () -> C.average_strategy c);
    cur = (fun () -> C.current_strategy c);
    reg = (fun () -> C.regrets c);
  }

let sampled seed weighting sampling (module G : Game.S) : engine =
  let module M = Mccfr.Make (G) in
  let m = M.create ~seed ~weighting sampling in
  {
    infosets = (M.table m).infosets;
    step = (fun () -> M.iterate m);
    count = (fun () -> M.iterations m);
    nodes = (fun () -> M.touched m);
    avg = (fun () -> M.average_strategy m);
    cur = (fun () -> M.current_strategy m);
    reg = (fun () -> M.regrets m);
  }

let create ?(algorithm = Cfr.Vanilla) ?sampling ?(seed = 1) ?(last = true)
    ?(budget = 4_000_000) ?(growth = 1.25) (module G : Game.S) =
  if growth <= 1. then invalid_arg "Session.create: growth must exceed 1";
  let module B = Best_response.Make (G) in
  let e =
    match sampling with
    | None -> full algorithm (module G)
    | Some s -> sampled seed algorithm s (module G)
  in
  let infosets = e.infosets in
  let n = Array.length infosets in
  let offset = Array.make (n + 1) 0 in
  Array.iteri
    (fun i (s : Tree.infoset) ->
      offset.(i + 1) <- offset.(i) + Array.length s.actions)
    infosets;
  let lookup = Hashtbl.create n in
  Array.iteri
    (fun i (s : Tree.infoset) -> Hashtbl.replace lookup s.key i)
    infosets;
  let flatten get =
    let a = Array.make offset.(n) 0. in
    Array.iteri
      (fun i (s : Tree.infoset) ->
        Array.blit (get i s) 0 a offset.(i) (Array.length s.actions))
      infosets;
    a
  in
  let snapshot () =
    let avg = e.avg () and cur = e.cur () in
    let reg = Array.of_list (List.map snd (e.reg ())) in
    let probs s _ (i : Tree.infoset) =
      Strategy.probs s i.key ~n:(Array.length i.actions)
    in
    (flatten (probs avg), flatten (probs cur), flatten (fun i _ -> reg.(i)))
  in
  let exploit () =
    let a = B.exploitability (e.avg ()) in
    (a, if last then Some (B.exploitability (e.cur ())) else None)
  in
  {
    game = (module G);
    average = e.avg;
    infosets;
    offset;
    lookup;
    step = e.step;
    iterations = e.count;
    touched = e.nodes;
    snapshot;
    exploit;
    budget;
    growth;
    next = 1;
    count = 0;
    stride = 1;
    points = [];
    frames = [];
  }

let game t = t.game
let infosets t = t.infosets
let average t = t.average ()
let iterations t = t.iterations ()
let touched t = t.touched ()
let slots t = t.offset.(Array.length t.infosets)

let rec thin t =
  if 3 * slots t * List.length t.frames > t.budget && List.length t.frames > 1
  then begin
    t.stride <- 2 * t.stride;
    t.frames <- List.filter (fun f -> f.index mod t.stride = 0) t.frames;
    thin t
  end

let record t =
  let at = t.iterations () in
  let average, current = t.exploit () in
  t.points <-
    { iteration = at; touched = t.touched (); average; current } :: t.points;
  if t.count mod t.stride = 0 then begin
    let avg, cur, reg = t.snapshot () in
    t.frames <- { index = t.count; at; avg; cur; reg } :: t.frames;
    thin t
  end;
  t.count <- t.count + 1

let iterate t =
  t.step ();
  let n = t.iterations () in
  if n >= t.next then begin
    record t;
    t.next <-
      max (n + 1) (int_of_float (Float.ceil (float_of_int n *. t.growth)))
  end

let checkpoint t =
  match t.points with
  | p :: _ when p.iteration = t.iterations () -> ()
  | _ -> record t

let points t = List.rev t.points
let frames t = List.rev_map (fun f -> f.at) t.frames

let row t i iteration (avg, cur, reg) =
  let o = t.offset.(i) and k = t.offset.(i + 1) - t.offset.(i) in
  {
    iteration;
    average = Array.sub avg o k;
    current = Array.sub cur o k;
    regret = Array.sub reg o k;
  }

let history t key =
  Hashtbl.find_opt t.lookup key
  |> Option.map (fun i ->
      List.rev_map (fun f -> row t i f.at (f.avg, f.cur, f.reg)) t.frames)

let table t =
  let s = t.snapshot () and at = t.iterations () in
  Array.init (Array.length t.infosets) (fun i -> row t i at s)
