open Multi_tree

type side = {
  player : int;
  first : int array;
  parent : int array;
  seq_comp : int array;
  plans : int array array array;
  has : bool array array array;
  local : int array;
}

type key = { ca : int; cb : int; sa : int; sb : int; so : int }

type t = {
  tree : Multi_tree.t;
  adversary : int;
  sides : side * side;
  seqs : int;
  first : int array;
  parent : int array;
  terms : (key * Rat.t) array;
}

type plan = { pa : int array; pb : int array }

type solution = {
  value : Rat.t;
  optimal : bool;
  support : (Rat.t * plan) list;
  columns : int;
  trace : (int * float * float) list;
  response : float array;
}

let limit = 1 lsl 16
let minus_one = Rat.of_int (-1)

let find uf i =
  let rec go i = if uf.(i) = i then i else go uf.(i) in
  let r = go i in
  let rec squash i =
    if uf.(i) <> r then begin
      let j = uf.(i) in
      uf.(i) <- r;
      squash j
    end
  in
  squash i;
  r

let enumerate (tree : Multi_tree.t) first kids ks =
  let rec plans = function
    | [] -> [ [] ]
    | k :: rest ->
        let here =
          List.concat_map
            (fun a ->
              List.map (fun p -> (k, a) :: p) (plans kids.(first.(k) + a)))
            (List.init (Array.length tree.labels.(k)) Fun.id)
        in
        let tail = plans rest in
        if List.length here * List.length tail > limit then
          invalid_arg "Team.build: too many local plans";
        List.concat_map (fun h -> List.map (fun tl -> h @ tl) tail) here
  in
  plans ks

let side (tree : Multi_tree.t) player first parent deals_of nseq =
  let n = Array.length tree.keys in
  let uf = Array.init n Fun.id in
  List.iter
    (fun ks ->
      match ks with
      | [] -> ()
      | k :: rest -> List.iter (fun k' -> uf.(find uf k') <- find uf k) rest)
    deals_of;
  let ids = Hashtbl.create 16 in
  let comp = Array.make n (-1) in
  Array.iteri
    (fun k p ->
      if p = player && first.(k) >= 0 then begin
        let r = find uf k in
        let c =
          match Hashtbl.find_opt ids r with
          | Some c -> c
          | None ->
              let c = Hashtbl.length ids in
              Hashtbl.add ids r c;
              c
        in
        comp.(k) <- c
      end)
    tree.owner;
  let ncomp = Hashtbl.length ids + 1 in
  let kids = Array.make nseq [] in
  for k = n - 1 downto 0 do
    if comp.(k) >= 0 then kids.(parent.(k)) <- k :: kids.(parent.(k))
  done;
  let local = Array.make nseq 0 in
  let counts = Array.make ncomp 0 in
  Array.iteri
    (fun k c ->
      if c >= 0 then
        for a = 0 to Array.length tree.labels.(k) - 1 do
          local.(first.(k) + a) <- counts.(c);
          counts.(c) <- counts.(c) + 1
        done)
    comp;
  let plans = Array.make ncomp [| [||] |]
  and has = Array.make ncomp [| [||] |] in
  for c = 0 to ncomp - 2 do
    let roots = List.filter (fun k -> comp.(k) = c) kids.(0) in
    let ps = Array.of_list (enumerate tree first kids roots) in
    plans.(c) <-
      Array.map
        (fun p -> Array.of_list (List.map (fun (k, a) -> first.(k) + a) p))
        ps;
    has.(c) <-
      Array.map
        (fun p ->
          let h = Array.make counts.(c) false in
          List.iter (fun (k, a) -> h.(local.(first.(k) + a)) <- true) p;
          h)
        ps
  done;
  let seq_comp = Array.make nseq (ncomp - 1) in
  Array.iteri
    (fun k c ->
      if c >= 0 then
        for a = 0 to Array.length tree.labels.(k) - 1 do
          seq_comp.(first.(k) + a) <- c
        done)
    comp;
  { player; first; parent; seq_comp; plans; has; local }

let dummy s = Array.length s.plans - 1

let build tree ~adversary =
  if tree.players <> 3 then invalid_arg "Team.build: three players";
  if adversary < 0 || adversary > 2 then invalid_arg "Team.build: adversary";
  let ma, mb =
    match List.filter (( <> ) adversary) [ 0; 1; 2 ] with
    | [ a; b ] -> (a, b)
    | _ -> assert false
  in
  let n = Array.length tree.keys in
  let first = Array.make n (-1) and parent = Array.make n (-1) in
  let next = Array.make 3 1 in
  let leaves = ref [] and deals = ref [] in
  let current = Array.make 3 [] in
  let rec go node reach seq inside =
    match node with
    | Leaf u ->
        if Float.abs (u.(0) +. u.(1) +. u.(2)) > 1e-9 then
          invalid_arg "Team.build: zero-sum games only";
        let team = Rat.recover (u.(ma) +. u.(mb)) in
        leaves := (Array.copy seq, Rat.mul reach team) :: !leaves
    | Chance (q, children) ->
        Array.iteri
          (fun j c -> go c (Rat.mul reach (Rat.recover q.(j))) seq inside)
          children
    | Decision (k, children) ->
        if not inside then begin
          deals := (current.(ma), current.(mb)) :: !deals;
          current.(ma) <- [];
          current.(mb) <- []
        end;
        let p = tree.owner.(k) in
        if first.(k) < 0 then begin
          first.(k) <- next.(p);
          parent.(k) <- seq.(p);
          next.(p) <- next.(p) + Array.length children
        end
        else if parent.(k) <> seq.(p) then
          invalid_arg "Team.build: imperfect recall";
        if p <> adversary then current.(p) <- k :: current.(p);
        Array.iteri
          (fun a c ->
            let seq' = Array.copy seq in
            seq'.(p) <- first.(k) + a;
            go c reach seq' true)
          children
  in
  go tree.root Rat.one [| 0; 0; 0 |] false;
  deals := (current.(ma), current.(mb)) :: !deals;
  let groups f = List.map f !deals in
  let first_of p =
    Array.mapi (fun k f -> if tree.owner.(k) = p then f else -1) first
  in
  let sa = side tree ma (first_of ma) parent (groups fst) next.(ma)
  and sb = side tree mb (first_of mb) parent (groups snd) next.(mb) in
  let acc = Hashtbl.create 1024 in
  List.iter
    (fun (seq, w) ->
      let key =
        {
          ca = sa.seq_comp.(seq.(ma));
          cb = sb.seq_comp.(seq.(mb));
          sa = seq.(ma);
          sb = seq.(mb);
          so = seq.(adversary);
        }
      in
      let old = Option.value ~default:Rat.zero (Hashtbl.find_opt acc key) in
      Hashtbl.replace acc key (Rat.add old w))
    !leaves;
  let keys =
    Hashtbl.fold (fun k v l -> if Rat.is_zero v then l else (k, v) :: l) acc []
    |> List.sort compare |> Array.of_list
  in
  {
    tree;
    adversary;
    sides = (sa, sb);
    seqs = next.(adversary);
    first = first_of adversary;
    parent;
    terms = keys;
  }

let members t =
  let a, b = t.sides in
  (a.player, b.player)

let local_plans t =
  let a, b = t.sides in
  (Array.map Array.length a.plans, Array.map Array.length b.plans)

let holds s c l seq = seq = 0 || s.has.(c).(l).(s.local.(seq))

let column t { pa; pb } =
  let a, b = t.sides in
  let c = Array.make t.seqs Rat.zero in
  Array.iter
    (fun (k, w) ->
      if holds a k.ca pa.(k.ca) k.sa && holds b k.cb pb.(k.cb) k.sb then
        c.(k.so) <- Rat.add c.(k.so) w)
    t.terms;
  c

let tables t y =
  let a, b = t.sides in
  let na = Array.length a.plans and nb = Array.length b.plans in
  let w = Array.make_matrix na nb [||] in
  Array.iter
    (fun (k, v) ->
      let x = Rat.to_float v *. y.(k.so) in
      if x <> 0. then begin
        if Array.length w.(k.ca).(k.cb) = 0 then
          w.(k.ca).(k.cb) <-
            Array.make_matrix
              (Array.length a.plans.(k.ca))
              (Array.length b.plans.(k.cb))
              0.;
        let m = w.(k.ca).(k.cb) in
        Array.iteri
          (fun la _ ->
            if holds a k.ca la k.sa then
              Array.iteri
                (fun lb _ ->
                  if holds b k.cb lb k.sb then m.(la).(lb) <- m.(la).(lb) +. x)
                m.(la))
          m
      end)
    t.terms;
  w

let objective w { pa; pb } =
  let v = ref 0. in
  Array.iteri
    (fun i row ->
      Array.iteri
        (fun j m -> if Array.length m > 0 then v := !v +. m.(pa.(i)).(pb.(j)))
        row)
    w;
  !v

let flip w =
  let ma = Array.length w and mb = Array.length w.(0) in
  Array.init mb (fun j ->
      Array.init ma (fun i ->
          let m = w.(i).(j) in
          if Array.length m = 0 then [||]
          else
            Array.init
              (Array.length m.(0))
              (fun lb -> Array.init (Array.length m) (fun la -> m.(la).(lb)))))

let prune w live_a live_b =
  let changed = ref false in
  Array.iteri
    (fun j lbs ->
      let alive = ref lbs in
      List.iter
        (fun lb ->
          let beats lb' =
            lb' <> lb
            &&
            let gap = ref 0. in
            Array.iteri
              (fun i row ->
                let m = row.(j) in
                if Array.length m > 0 then
                  gap :=
                    !gap
                    +. List.fold_left
                         (fun g la -> Float.min g (m.(la).(lb') -. m.(la).(lb)))
                         Float.infinity live_a.(i))
              w;
            !gap >= 0.
          in
          if List.exists beats !alive then begin
            alive := List.filter (( <> ) lb) !alive;
            changed := true
          end)
        lbs;
      live_b.(j) <- !alive)
    live_b;
  !changed

let reduce w na nb =
  let live_a = Array.map (fun n -> List.init n Fun.id) na
  and live_b = Array.map (fun n -> List.init n Fun.id) nb in
  let wt = flip w in
  let rec go () =
    let b = prune w live_a live_b in
    let a = prune wt live_b live_a in
    if a || b then go ()
  in
  go ();
  let keep_a = Array.map Array.of_list live_a
  and keep_b = Array.map Array.of_list live_b in
  let sub =
    Array.mapi
      (fun i row ->
        Array.mapi
          (fun j m ->
            if Array.length m = 0 then [||]
            else
              Array.map
                (fun la -> Array.map (fun lb -> m.(la).(lb)) keep_b.(j))
                keep_a.(i))
          row)
      w
  in
  (sub, keep_a, keep_b)

let best_labels s =
  Array.map
    (fun r ->
      let b = ref 0 in
      Array.iteri (fun l x -> if x > r.(!b) then b := l) r;
      !b)
    s

let fit w fixed =
  let ma = Array.length w in
  let s =
    Array.init ma (fun i ->
        let n = Array.fold_left (fun n m -> max n (Array.length m)) 0 w.(i) in
        let r = Array.make (max n 1) 0. in
        Array.iteri
          (fun j m ->
            if Array.length m > 0 then
              Array.iteri (fun la row -> r.(la) <- r.(la) +. row.(fixed.(j))) m)
          w.(i);
        r)
  in
  best_labels s

let ascend w wt start =
  let rec go p v =
    let pa = fit w p.pb in
    let pb = fit wt pa in
    let p' = { pa; pb } in
    let v' = objective w p' in
    if v' > v +. 1e-12 then go p' v' else (p, v)
  in
  go start (objective w start)

let search ?(budget = max_int) w na nb floor =
  let ma = Array.length na and mb = Array.length nb in
  let tm = Array.init mb (fun j -> Array.init ma (fun i -> w.(i).(j))) in
  let opt =
    Array.init mb (fun j ->
        Array.init ma (fun i ->
            Array.map (Array.fold_left Float.max Float.neg_infinity) tm.(j).(i)))
  in
  let spread j =
    Array.fold_left
      (fun acc o ->
        if Array.length o = 0 then acc
        else
          acc
          +. Array.fold_left Float.max Float.neg_infinity o
          -. Array.fold_left Float.min Float.infinity o)
      0. opt.(j)
  in
  let order = Array.init mb Fun.id in
  Array.stable_sort (fun x y -> Float.compare (spread y) (spread x)) order;
  let solo =
    Array.init mb (fun j ->
        Array.fold_left Float.max Float.neg_infinity
          (Array.init nb.(j) (fun lb ->
               Array.fold_left
                 (fun acc m ->
                   if Array.length m = 0 then acc
                   else
                     acc
                     +. Array.fold_left
                          (fun x row -> Float.max x row.(lb))
                          Float.neg_infinity m)
                 0. tm.(j))))
  in
  let s = Array.init ma (fun i -> Array.make na.(i) 0.) in
  let fixed = Array.init ma (fun i -> Array.make na.(i) 0.) in
  Array.iter
    (Array.iteri (fun i o ->
         Array.iteri (fun l x -> s.(i).(l) <- s.(i).(l) +. x) o))
    opt;
  let rest = ref (Array.fold_left ( +. ) 0. solo) in
  let top r = Array.fold_left Float.max Float.neg_infinity r in
  let bound () =
    let a = Array.fold_left (fun acc r -> acc +. top r) 0. s in
    let b = Array.fold_left (fun acc r -> acc +. top r) !rest fixed in
    Float.min a b
  in
  let shift j lb sign =
    rest := !rest -. (sign *. solo.(j));
    Array.iteri
      (fun i m ->
        Array.iteri
          (fun l row ->
            s.(i).(l) <- s.(i).(l) +. (sign *. (row.(lb) -. opt.(j).(i).(l)));
            fixed.(i).(l) <- fixed.(i).(l) +. (sign *. row.(lb)))
          m)
      tm.(j)
  in
  let best = ref None and bv = ref floor in
  let nodes = ref 0 and complete = ref true in
  let pb = Array.make mb 0 in
  let rec dive d =
    incr nodes;
    if !nodes > budget then complete := false
    else if d = mb then begin
      let v = Array.fold_left (fun acc r -> acc +. top r) 0. s in
      if v > !bv then begin
        bv := v;
        best := Some { pa = best_labels s; pb = Array.copy pb }
      end
    end
    else begin
      let j = order.(d) in
      List.init nb.(j) (fun lb ->
          shift j lb 1.;
          let v = bound () in
          shift j lb (-1.);
          (v, lb))
      |> List.sort (fun (x, _) (y, _) -> Float.compare y x)
      |> List.iter (fun (v, lb) ->
          if v > !bv +. 1e-12 && !complete then begin
            shift j lb 1.;
            pb.(j) <- lb;
            dive (d + 1);
            shift j lb (-1.)
          end)
    end
  in
  dive 0;
  (!best, !bv, !complete)

let respond ?budget ?(starts = []) t y =
  let na, nb = local_plans t in
  let full = tables t y in
  let zero =
    { pa = Array.map (fun _ -> 0) na; pb = Array.map (fun _ -> 0) nb }
  in
  let fullt = flip full in
  let incumbent, floor =
    List.fold_left
      (fun (p, v) start ->
        let p', v' = ascend full fullt start in
        if v' > v then (p', v') else (p, v))
      (ascend full fullt zero) starts
  in
  let w, keep_a, keep_b = reduce full na nb in
  let size keep =
    Array.fold_left
      (fun acc k -> acc +. Float.log (float (Array.length k)))
      0. keep
  in
  let swap = size keep_a < size keep_b in
  let w = if swap then flip w else w in
  let ka, kb = if swap then (keep_b, keep_a) else (keep_a, keep_b) in
  let found, v, complete =
    search ?budget w
      (Array.map Array.length ka)
      (Array.map Array.length kb)
      floor
  in
  let lift keep l = Array.mapi (fun i x -> keep.(i).(x)) l in
  let p =
    match found with
    | None -> incumbent
    | Some p when swap -> { pa = lift keep_a p.pb; pb = lift keep_b p.pa }
    | Some p -> { pa = lift keep_a p.pa; pb = lift keep_b p.pb }
  in
  (v, p, complete)

let rows t =
  let open Lp in
  let eq coeffs rhs = { coeffs; rel = Eq; rhs } in
  let root = eq [ (0, Rat.one) ] Rat.one in
  let flows =
    Array.to_list
      (Array.mapi
         (fun k f ->
           if f < 0 then None
           else
             let n = Array.length t.tree.labels.(k) in
             Some
               (eq
                  ((t.parent.(k), minus_one)
                  :: List.init n (fun a -> (f + a, Rat.one)))
                  Rat.zero))
         t.first)
    |> List.filter_map Fun.id
  in
  root :: flows

let restricted ~exact t cols =
  let s = t.seqs in
  let wp = s and wm = s + 1 in
  let cut c =
    {
      Lp.coeffs =
        (wp, minus_one) :: (wm, Rat.one)
        :: List.filter_map
             (fun (i, x) -> if Rat.is_zero x then None else Some (i, x))
             (List.mapi (fun i x -> (i, x)) (Array.to_list c));
      rel = Le;
      rhs = Rat.zero;
    }
  in
  let objective = [ (wp, minus_one); (wm, Rat.one) ]
  and constraints = rows t @ List.map cut cols in
  if exact then
    match Lp.maximize ~vars:(s + 2) objective constraints with
    | Optimal { value; x; _ } ->
        (-.Rat.to_float value, Array.init s (fun i -> Rat.to_float x.(i)))
    | Infeasible | Unbounded -> invalid_arg "Team: restricted LP"
  else
    match Lp.maximize_float ~vars:(s + 2) objective constraints with
    | Some (value, x) -> (-.value, Array.sub x 0 s)
    | None -> invalid_arg "Team: restricted LP"

let mixture t cols =
  let s = t.seqs in
  let infos =
    Array.to_list t.first
    |> List.mapi (fun k f -> (k, f))
    |> List.filter (fun (_, f) -> f >= 0)
  in
  let nv = 2 + (2 * List.length infos) in
  let var = Hashtbl.create 64 in
  List.iteri (fun j (k, _) -> Hashtbl.add var k (2 + (2 * j))) infos;
  let terms = Array.make s [ (0, Rat.one); (1, minus_one) ] in
  for i = 1 to s - 1 do
    terms.(i) <- []
  done;
  List.iter
    (fun (k, f) ->
      let v = Hashtbl.find var k in
      let pm sign = [ (v, sign); (v + 1, Rat.neg sign) ] in
      terms.(t.parent.(k)) <- pm minus_one @ terms.(t.parent.(k));
      for a = 0 to Array.length t.tree.labels.(k) - 1 do
        terms.(f + a) <- pm Rat.one @ terms.(f + a)
      done)
    infos;
  let ncols = List.length cols in
  let lam i = nv + i in
  let carr = Array.of_list cols in
  let row s' =
    {
      Lp.coeffs =
        terms.(s')
        @ List.filter_map
            (fun i ->
              let x = carr.(i).(s') in
              if Rat.is_zero x then None else Some (lam i, Rat.neg x))
            (List.init ncols Fun.id);
      rel = Le;
      rhs = Rat.zero;
    }
  in
  let total =
    {
      Lp.coeffs = List.init ncols (fun i -> (lam i, Rat.one));
      rel = Eq;
      rhs = Rat.one;
    }
  in
  match
    Lp.maximize ~vars:(nv + ncols)
      [ (0, Rat.one); (1, minus_one) ]
      (total :: List.init s row)
  with
  | Optimal { value; x; _ } -> (value, Array.init ncols (fun i -> x.(lam i)))
  | Infeasible | Unbounded -> invalid_arg "Team: master LP"

let guarantee t support =
  let c = Array.make t.seqs Rat.zero in
  List.iter
    (fun (l, p) ->
      Array.iteri (fun i x -> c.(i) <- Rat.add c.(i) (Rat.mul l x)) (column t p))
    support;
  let kids = Array.make t.seqs [] in
  Array.iteri
    (fun k f -> if f >= 0 then kids.(t.parent.(k)) <- k :: kids.(t.parent.(k)))
    t.first;
  let rec value s =
    List.fold_left
      (fun acc k ->
        let best = ref None in
        for a = 0 to Array.length t.tree.labels.(k) - 1 do
          let v = value (t.first.(k) + a) in
          best := Some (match !best with None -> v | Some b -> Rat.min b v)
        done;
        Rat.add acc (Option.get !best))
      c.(s) kids.(s)
  in
  value 0

let solve ?budget ?(iterations = 10_000) t =
  let na, nb = local_plans t in
  let start =
    { pa = Array.make (Array.length na) 0; pb = Array.make (Array.length nb) 0 }
  in
  let rec loop i plans cols trace exact =
    let lo, y = restricted ~exact t cols in
    let up, p, complete = respond ?budget ~starts:plans t y in
    let improves = up > lo +. 1e-9 && not (List.mem p plans) in
    if complete && improves && i < iterations then
      loop (i + 1) (p :: plans) (column t p :: cols) ((i, lo, up) :: trace)
        false
    else if complete && not exact then loop i plans cols trace true
    else (plans, cols, complete, y, List.rev ((i, lo, up) :: trace))
  in
  let plans, cols, optimal, yf, trace =
    loop 1 [ start ] [ column t start ] [] false
  in
  let value, lam = mixture t cols in
  let support =
    List.combine (Array.to_list lam) plans
    |> List.filter (fun (l, _) -> Rat.sign l > 0)
  in
  { value; optimal; support; columns = List.length cols; trace; response = yf }

let pure t s p =
  let tree = t.tree in
  let chosen = Hashtbl.create 64 in
  Array.iteri
    (fun c l ->
      if c < dummy s then
        Array.iter (fun q -> Hashtbl.replace chosen q ()) s.plans.(c).(l))
    p;
  let out = ref Strategy.empty in
  Array.iteri
    (fun k key ->
      if tree.owner.(k) = s.player then begin
        let n = Array.length tree.labels.(k) in
        let a =
          List.find_opt
            (fun a -> Hashtbl.mem chosen (s.first.(k) + a))
            (List.init n Fun.id)
          |> Option.value ~default:0
        in
        out :=
          Strategy.add key ~actions:tree.labels.(k)
            ~probs:(Array.init n (fun j -> if j = a then 1. else 0.))
            !out
      end)
    tree.keys;
  !out

let strategies t { pa; pb } =
  let a, b = t.sides in
  Strategy.union (pure t a pa) (pure t b pb)

let marginal t s pick support =
  let tree = t.tree in
  let reach = Hashtbl.create 64 in
  List.iter
    (fun (l, p) ->
      let lf = Rat.to_float l in
      Array.iteri
        (fun c lc ->
          if c < dummy s then
            Array.iter
              (fun q ->
                Hashtbl.replace reach q
                  (lf +. Option.value ~default:0. (Hashtbl.find_opt reach q)))
              s.plans.(c).(lc))
        (pick p))
    support;
  let r q =
    if q = 0 then 1. else Option.value ~default:0. (Hashtbl.find_opt reach q)
  in
  let out = ref Strategy.empty in
  Array.iteri
    (fun k key ->
      if tree.owner.(k) = s.player then begin
        let n = Array.length tree.labels.(k) in
        let above = r s.parent.(k) in
        let probs =
          if above <= 0. then Array.make n (1. /. float n)
          else Array.init n (fun a -> r (s.first.(k) + a) /. above)
        in
        out := Strategy.add key ~actions:tree.labels.(k) ~probs !out
      end)
    tree.keys;
  !out

let marginals t support =
  let a, b = t.sides in
  Strategy.union
    (marginal t a (fun p -> p.pa) support)
    (marginal t b (fun p -> p.pb) support)

let realization t sigma =
  let tree = t.tree in
  let y = Array.make t.seqs 0. in
  y.(0) <- 1.;
  let rec go node seq =
    match node with
    | Leaf _ -> ()
    | Chance (_, children) -> Array.iter (fun c -> go c seq) children
    | Decision (k, children) ->
        if tree.owner.(k) = t.adversary then begin
          let n = Array.length children in
          let p = Strategy.probs sigma tree.keys.(k) ~n in
          Array.iteri
            (fun a c ->
              let s = t.first.(k) + a in
              y.(s) <- y.(seq) *. p.(a);
              go c s)
            children
        end
        else Array.iter (fun c -> go c seq) children
  in
  go tree.root 0;
  y
