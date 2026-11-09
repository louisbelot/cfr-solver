type side = {
  keys : string array;
  actions : string array array;
  parent : int array;
  first : int array;
  children : int list array;
  owner : int array;
}

type entry = { row : int; col : int; reach : Rat.t; a : Rat.t; b : Rat.t }
type t = { sides : side array; payoff : entry array; top : Rat.t array }
type found = { key : string; labels : string array; up : int; start : int }

let build (module G : Multi.S) =
  if G.players <> 2 then invalid_arg "Lemke.build: two players";
  let ids = [| Hashtbl.create 64; Hashtbl.create 64 |] in
  let found = [| []; [] |] and count = [| 0; 0 |] and next = [| 1; 1 |] in
  let table = Hashtbl.create 1024 in
  let top = [| None; None |] in
  let rec go h reach s0 s1 =
    match G.node h with
    | Multi.Terminal u ->
        let u = Array.map Rat.of_float u in
        Array.iteri
          (fun p v ->
            top.(p) <-
              Some (match top.(p) with None -> v | Some m -> Rat.max m v))
          u;
        let r, a, b =
          Option.value
            ~default:(Rat.zero, Rat.zero, Rat.zero)
            (Hashtbl.find_opt table (s0, s1))
        in
        Hashtbl.replace table (s0, s1)
          ( Rat.add r reach,
            Rat.add a (Rat.mul reach u.(0)),
            Rat.add b (Rat.mul reach u.(1)) )
    | Chance outcomes ->
        let ps = List.map (fun (p, _) -> Rat.recover p) outcomes in
        if not (Rat.equal (List.fold_left Rat.add Rat.zero ps) Rat.one) then
          invalid_arg "Lemke.build: chance probabilities";
        List.iter2 (fun p (_, c) -> go c (Rat.mul reach p) s0 s1) ps outcomes
    | Decision { player = p; infoset; actions } ->
        let s = if p = 0 then s0 else s1 in
        let f =
          match Hashtbl.find_opt ids.(p) infoset with
          | Some (up, start) ->
              if up <> s then invalid_arg "Lemke.build: imperfect recall";
              start
          | None ->
              let start = next.(p) in
              Hashtbl.add ids.(p) infoset (s, start);
              found.(p) <-
                {
                  key = infoset;
                  labels = Array.of_list (List.map fst actions);
                  up = s;
                  start;
                }
                :: found.(p);
              count.(p) <- count.(p) + 1;
              next.(p) <- start + List.length actions;
              start
        in
        List.iteri
          (fun a (_, c) ->
            if p = 0 then go c reach (f + a) s1 else go c reach s0 (f + a))
          actions
  in
  go G.root Rat.one 0 0;
  let side p =
    let infos = Array.of_list (List.rev found.(p)) in
    let children = Array.make next.(p) [] and owner = Array.make next.(p) 0 in
    Array.iteri
      (fun j i ->
        children.(i.up) <- j :: children.(i.up);
        Array.iteri (fun a _ -> owner.(i.start + a) <- 1 + j) i.labels)
      infos;
    {
      keys = Array.map (fun i -> i.key) infos;
      actions = Array.map (fun i -> i.labels) infos;
      parent = Array.map (fun i -> i.up) infos;
      first = Array.map (fun i -> i.start) infos;
      children = Array.map List.rev children;
      owner;
    }
  in
  let payoff =
    Hashtbl.fold
      (fun (row, col) (reach, a, b) acc -> { row; col; reach; a; b } :: acc)
      table []
    |> List.sort (fun e e' -> compare (e.row, e.col) (e'.row, e'.col))
    |> Array.of_list
  in
  {
    sides = [| side 0; side 1 |];
    payoff;
    top = Array.map (Option.value ~default:Rat.zero) top;
  }

let sequences t p = Array.length t.sides.(p).owner
let infosets t p = Array.length t.sides.(p).keys

let realize exact t p sigma =
  let s = t.sides.(p) in
  let x = Array.make (Array.length s.owner) Rat.zero in
  x.(0) <- Rat.one;
  Array.iteri
    (fun j key ->
      let n = Array.length s.actions.(j) in
      let w = Array.map exact (Strategy.probs sigma key ~n) in
      let total = Array.fold_left Rat.add Rat.zero w in
      let reach = x.(s.parent.(j)) in
      Array.iteri
        (fun a wa ->
          x.(s.first.(j) + a) <-
            (if Rat.is_zero total then Rat.div reach (Rat.of_int n)
             else Rat.mul reach (Rat.div wa total)))
        w)
    s.keys;
  x

let plan = realize Rat.of_float
let percent q = Rat.of_ints (Float.to_int (Float.round (q *. 100.))) 100

let behavior t p x =
  let s = t.sides.(p) in
  List.init (Array.length s.keys) (fun j ->
      let n = Array.length s.actions.(j) and reach = x.(s.parent.(j)) in
      ( s.keys.(j),
        Array.init n (fun a ->
            if Rat.is_zero reach then Rat.of_ints 1 n
            else Rat.div x.(s.first.(j) + a) reach) ))

let strategy t (x, y) =
  List.fold_left
    (fun acc (p, plan) ->
      List.fold_left2
        (fun acc (key, probs) actions ->
          Strategy.add key ~actions ~probs:(Array.map Rat.to_float probs) acc)
        acc (behavior t p plan)
        (Array.to_list t.sides.(p).actions))
    Strategy.empty
    [ (0, x); (1, y) ]

let respond s g =
  let n = Array.length s.keys in
  let best = Array.make n Rat.zero and pick = Array.make n 0 in
  let below seq =
    List.fold_left (fun acc j -> Rat.add acc best.(j)) g.(seq) s.children.(seq)
  in
  for j = n - 1 downto 0 do
    let f = s.first.(j) in
    let v = ref (below f) in
    for a = 1 to Array.length s.actions.(j) - 1 do
      let w = below (f + a) in
      if Rat.compare w !v > 0 then (
        v := w;
        pick.(j) <- a)
    done;
    best.(j) <- !v
  done;
  (below 0, pick)

let gradients t ~a ~b (x, y) =
  let gx = Array.make (sequences t 0) Rat.zero
  and gy = Array.make (sequences t 1) Rat.zero in
  Array.iter
    (fun e ->
      gx.(e.row) <- Rat.add gx.(e.row) (Rat.mul (a e) y.(e.col));
      gy.(e.col) <- Rat.add gy.(e.col) (Rat.mul (b e) x.(e.row)))
    t.payoff;
  (gx, gy)

let values t (x, y) =
  Array.fold_left
    (fun (u, v) e ->
      let w = Rat.mul x.(e.row) y.(e.col) in
      (Rat.add u (Rat.mul e.a w), Rat.add v (Rat.mul e.b w)))
    (Rat.zero, Rat.zero) t.payoff

let gains t xy =
  let gx, gy = gradients t ~a:(fun e -> e.a) ~b:(fun e -> e.b) xy in
  let u, v = values t xy in
  ( Rat.sub (fst (respond t.sides.(0) gx)) u,
    Rat.sub (fst (respond t.sides.(1) gy)) v )

type solution = {
  plans : Rat.t array * Rat.t array;
  values : Rat.t * Rat.t;
  strategy : Strategy.t;
  pivots : int;
}

let solve ?(prior = Strategy.empty) ?(max_pivots = 1_000_000) t =
  let s0 = t.sides.(0) and s1 = t.sides.(1) in
  let n1 = sequences t 0 and n2 = sequences t 1 in
  let k1 = 1 + infosets t 0 and k2 = 1 + infosets t 1 in
  let shift p e v = Rat.sub v (Rat.mul (Rat.add t.top.(p) Rat.one) e.reach) in
  let a e = shift 0 e e.a and b e = shift 1 e e.b in
  let x0 = realize percent t 0 prior and y0 = realize percent t 1 prior in
  let ax0, by0 = gradients t ~a ~b (x0, y0) in
  let cx i = i and cy j = n1 + j in
  let cwx i = n1 + n2 + i and cwy j = (2 * n1) + n2 + j in
  let cp k = (2 * n1) + (2 * n2) + k and cq k = (2 * n1) + (2 * n2) + k1 + k in
  let cz = (2 * n1) + (2 * n2) + k1 + k2 in
  let vars = cz + 1 and m = n1 + n2 + k1 + k2 in
  let rhs = vars + m and width = vars + m + 1 in
  let tab = Array.init m (fun _ -> Array.make width Rat.zero) in
  let bump r c v = tab.(r).(c) <- Rat.add tab.(r).(c) v in
  let treeplex s ~seq ~dual ~row0 ~slack ~gz ~rows =
    let n = Array.length s.owner in
    for i = 0 to n - 1 do
      bump (row0 + i) (slack i) Rat.one;
      bump (row0 + i) (dual s.owner.(i)) (Rat.neg Rat.one);
      List.iter (fun j -> bump (row0 + i) (dual (1 + j)) Rat.one) s.children.(i);
      bump (row0 + i) cz gz.(i)
    done;
    bump rows (seq 0) Rat.one;
    bump rows cz Rat.one;
    bump rows rhs Rat.one;
    Array.iteri
      (fun j par ->
        bump (rows + 1 + j) (seq par) (Rat.neg Rat.one);
        Array.iteri
          (fun a _ -> bump (rows + 1 + j) (seq (s.first.(j) + a)) Rat.one)
          s.actions.(j))
      s.parent
  in
  treeplex s0 ~seq:cx ~dual:cp ~row0:0 ~slack:cwx ~gz:ax0 ~rows:(n1 + n2);
  treeplex s1 ~seq:cy ~dual:cq ~row0:n1 ~slack:cwy ~gz:by0 ~rows:(n1 + n2 + k1);
  Array.iter
    (fun e ->
      bump e.row (cy e.col) (a e);
      bump (n1 + e.col) (cx e.row) (b e))
    t.payoff;
  let basis = Array.make m (-1) and row_of = Array.make vars (-1) in
  let pivot r c =
    let row = tab.(r) in
    let inv = Rat.inv row.(c) in
    let nz = ref [] in
    for k = width - 1 downto 0 do
      if not (Rat.is_zero row.(k)) then (
        row.(k) <- Rat.mul row.(k) inv;
        nz := k :: !nz)
    done;
    let nz = Array.of_list !nz in
    Array.iteri
      (fun i other ->
        if i <> r then
          let f = other.(c) in
          if not (Rat.is_zero f) then
            Array.iter
              (fun k -> other.(k) <- Rat.sub other.(k) (Rat.mul f row.(k)))
              nz)
      tab;
    if basis.(r) >= 0 then row_of.(basis.(r)) <- -1;
    basis.(r) <- c;
    row_of.(c) <- r
  in
  let install c =
    let r = ref 0 in
    while !r < m && (basis.(!r) >= 0 || Rat.is_zero tab.(!r).(c)) do
      incr r
    done;
    if !r = m then failwith "Lemke.solve: singular start";
    pivot !r c
  in
  let chosen s g =
    let _, pick = respond s g in
    let on = Array.make (Array.length s.owner) false in
    on.(0) <- true;
    Array.iteri (fun j f -> on.(f + pick.(j)) <- true) s.first;
    on
  in
  let bx = chosen s0 ax0 and by = chosen s1 by0 in
  install cz;
  for k = 0 to k1 - 1 do
    install (cp k)
  done;
  for k = 0 to k2 - 1 do
    install (cq k)
  done;
  for i = 1 to n1 - 1 do
    install (if bx.(i) then cx i else cwx i)
  done;
  for j = 0 to n2 - 1 do
    install (if by.(j) then cy j else cwy j)
  done;
  let free c = c >= cp 0 && c < cz in
  Array.iteri
    (fun r c ->
      if (not (free c)) && Rat.sign tab.(r).(rhs) < 0 then
        failwith "Lemke.solve: infeasible start")
    basis;
  for r = 0 to m - 1 do
    tab.(r).(vars + r) <- Rat.one
  done;
  let compare_rows c r r' =
    let d = tab.(r).(c) and d' = tab.(r').(c) in
    let cmp k = Rat.compare (Rat.mul tab.(r).(k) d') (Rat.mul tab.(r').(k) d) in
    match cmp rhs with
    | 0 when basis.(r) = cz -> -1
    | 0 when basis.(r') = cz -> 1
    | 0 ->
        let k = ref vars in
        while !k < rhs && cmp !k = 0 do
          incr k
        done;
        if !k = rhs then failwith "Lemke.solve: lexicographic tie";
        cmp !k
    | o -> o
  in
  let complement c = if c < n1 + n2 then c + n1 + n2 else c - n1 - n2 in
  let rec walk entering count =
    if count >= max_pivots then failwith "Lemke.solve: pivot limit";
    let best = ref (-1) in
    for r = 0 to m - 1 do
      if
        (not (free basis.(r)))
        && Rat.sign tab.(r).(entering) > 0
        && (!best < 0 || compare_rows entering r !best < 0)
      then best := r
    done;
    if !best < 0 then failwith "Lemke.solve: ray";
    let leaving = basis.(!best) in
    pivot !best entering;
    if leaving = cz then count + 1 else walk (complement leaving) (count + 1)
  in
  let pivots = walk (cx 0) 0 in
  let read c = if row_of.(c) >= 0 then tab.(row_of.(c)).(rhs) else Rat.zero in
  let x = Array.init n1 (fun i -> read (cx i))
  and y = Array.init n2 (fun j -> read (cy j)) in
  {
    plans = (x, y);
    values = values t (x, y);
    strategy = strategy t (x, y);
    pivots;
  }
