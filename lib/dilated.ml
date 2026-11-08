let response ~offset ~parent ~order ~eta g =
  let b = Array.make (Array.length g) 0.
  and below = Array.make (Array.length g) 0. in
  Array.iter
    (fun i ->
      let lo = offset.(i) and hi = offset.(i + 1) - 1 in
      let z k = eta *. (g.(k) +. below.(k)) in
      let m = ref neg_infinity in
      for k = lo to hi do
        m := Float.max !m (z k)
      done;
      let s = ref 0. in
      for k = lo to hi do
        b.(k) <- Float.exp (z k -. !m);
        s := !s +. b.(k)
      done;
      for k = lo to hi do
        b.(k) <- b.(k) /. !s
      done;
      let p = parent.(i) in
      if p >= 0 then below.(p) <- below.(p) +. ((!m +. Float.log !s) /. eta))
    order;
  b

module Make (G : Game.S) = struct
  type t = {
    root : Tree.node;
    table : Table.t;
    size : int;
    parent : int array;
    order : int array;
    eta : float;
    optimistic : bool;
    total : float array;
    last : float array;
    grad : float array;
    seq : float array;
    mutable iterations : int;
    mutable touched : int;
  }

  let sequences (tree : Tree.t) (table : Table.t) =
    let n = Array.length tree.infosets in
    let parent = Array.make n (-1) and depth = Array.make n 0 in
    let rec go node s0 s1 d0 d1 =
      match node with
      | Tree.Terminal _ -> ()
      | Chance { children; _ } ->
          Array.iter (fun c -> go c s0 s1 d0 d1) children
      | Decision { infoset = i; children } ->
          let base = table.offset.(i) in
          Array.iteri
            (fun a c ->
              match table.player.(i) with
              | Game.P0 ->
                  parent.(i) <- s0;
                  depth.(i) <- d0;
                  go c (base + a) s1 (d0 + 1) d1
              | P1 ->
                  parent.(i) <- s1;
                  depth.(i) <- d1;
                  go c s0 (base + a) d0 (d1 + 1))
            children
    in
    go tree.root (-1) (-1) 0 0;
    let order = Array.init n Fun.id in
    Array.stable_sort (fun i j -> compare depth.(j) depth.(i)) order;
    (parent, order)

  let solve t =
    let w = if t.optimistic then 1. else 0. in
    let g = Array.mapi (fun k x -> x +. (w *. t.last.(k))) t.total in
    let b =
      response ~offset:t.table.offset ~parent:t.parent ~order:t.order ~eta:t.eta
        g
    in
    Array.blit b 0 t.table.current 0 (Array.length b)

  let create ?(optimistic = true) ~eta () =
    if not (eta > 0.) then invalid_arg "Dilated.create: eta must be positive";
    let tree = Tree.build (module G) in
    let table = Table.create tree in
    let parent, order = sequences tree table in
    let slots = Table.slots table in
    let t =
      {
        root = tree.root;
        table;
        size = Tree.size tree.root;
        parent;
        order;
        eta;
        optimistic;
        total = Array.make slots 0.;
        last = Array.make slots 0.;
        grad = Array.make slots 0.;
        seq = Array.make slots 0.;
        iterations = 0;
        touched = 0;
      }
    in
    solve t;
    t

  let iterations t = t.iterations
  let touched t = t.touched

  let rec walk t node r0 r1 pc s0 s1 =
    match node with
    | Tree.Terminal u ->
        if s0 >= 0 then t.grad.(s0) <- t.grad.(s0) +. (u *. pc *. r1);
        if s1 >= 0 then t.grad.(s1) <- t.grad.(s1) -. (u *. pc *. r0)
    | Chance { probs; children } ->
        Array.iteri (fun k c -> walk t c r0 r1 (pc *. probs.(k)) s0 s1) children
    | Decision { infoset = i; children } ->
        let tab = t.table in
        let base = tab.offset.(i) in
        Array.iteri
          (fun a c ->
            let b = tab.current.(base + a) in
            match tab.player.(i) with
            | Game.P0 -> walk t c (r0 *. b) r1 pc (base + a) s1
            | P1 -> walk t c r0 (r1 *. b) pc s0 (base + a))
          children

  let accumulate t =
    let tab = t.table in
    for j = Array.length t.order - 1 downto 0 do
      let i = t.order.(j) in
      let p = t.parent.(i) in
      let reach = if p >= 0 then t.seq.(p) else 1. in
      for k = tab.offset.(i) to tab.offset.(i + 1) - 1 do
        t.seq.(k) <- reach *. tab.current.(k);
        tab.strategy_sum.(k) <- tab.strategy_sum.(k) +. t.seq.(k)
      done
    done

  let iterate t =
    accumulate t;
    Array.fill t.grad 0 (Array.length t.grad) 0.;
    walk t t.root 1. 1. 1. (-1) (-1);
    for k = 0 to Array.length t.grad - 1 do
      t.total.(k) <- t.total.(k) +. t.grad.(k);
      t.last.(k) <- t.grad.(k)
    done;
    solve t;
    t.touched <- t.touched + t.size;
    t.iterations <- t.iterations + 1

  let run t n =
    for _ = 1 to n do
      iterate t
    done

  let average_strategy t = Table.average_strategy t.table
  let current_strategy t = Table.behavior t.table
end
