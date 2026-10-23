type mode = Shared | Private

let mode_name = function Shared -> "shared" | Private -> "private"

module Make (G : Game.S) = struct
  module M = Mccfr.Make (G)

  type t = {
    mode : mode;
    workers : M.t array;
    master : Table.t;
    weighting : Cfr.algorithm;
    batch : int;
    pool : Pool.t;
    mutable placed : bool;
    mutable rounds : int;
  }

  let create ?(seed = 1) ?(weighting = Cfr.Vanilla) ?(batch = 1000)
      ?(mode = Shared) ?(pool = Pool.default ()) ~domains sampling =
    if domains < 1 then invalid_arg "Parallel.create: domains must be positive";
    if batch < 1 then invalid_arg "Parallel.create: batch must be positive";
    let first = M.create ~seed sampling and shared = mode = Shared in
    {
      mode;
      workers =
        Array.init domains (fun j ->
            if j = 0 then first else M.split ~shared first);
      master = (if shared then M.table first else Table.copy (M.table first));
      weighting;
      batch;
      pool;
      placed = false;
      rounds = 0;
    }

  let domains t = Array.length t.workers
  let rounds t = t.rounds

  let merge t lo hi =
    let m = t.master and n = Array.length t.workers in
    match t.mode with
    | Shared ->
        for k = lo to hi - 1 do
          let s = ref m.strategy_sum.(k) in
          for j = 0 to n - 1 do
            let w = M.table t.workers.(j) in
            s := !s +. w.strategy_sum.(k);
            w.strategy_sum.(k) <- 0.
          done;
          m.strategy_sum.(k) <- !s
        done
    | Private ->
        let w0 = M.table t.workers.(0) in
        for k = lo to hi - 1 do
          let r = ref w0.regret.(k) and s = ref w0.strategy_sum.(k) in
          for j = 1 to n - 1 do
            let w = M.table t.workers.(j) in
            r := !r +. (w.regret.(k) -. m.regret.(k));
            s := !s +. (w.strategy_sum.(k) -. m.strategy_sum.(k))
          done;
          m.regret.(k) <- !r;
          m.strategy_sum.(k) <- !s
        done

  let round_size t n =
    match (t.mode, t.weighting) with
    | Shared, Cfr.Vanilla -> max n 1
    | _ -> t.batch * domains t

  let work t barrier n j =
    let d = domains t and slots = Table.slots t.master in
    let lo = j * slots / d and hi = (j + 1) * slots / d in
    let shared = t.mode = Shared and per = round_size t n in
    if not t.placed then t.workers.(j) <- M.copy ~shared t.workers.(j);
    let w = t.workers.(j) in
    let rec loop left round =
      if left > 0 then begin
        let k = min left per in
        M.run w ((k / d) + if j < k mod d then 1 else 0);
        Barrier.await barrier;
        merge t lo hi;
        Table.discount_range t.master lo hi (Cfr.discounts t.weighting round);
        Barrier.await barrier;
        if not shared then Table.blit ~src:t.master ~dst:(M.table w);
        loop (left - k) (round + 1)
      end
    in
    loop n (t.rounds + 1)

  let run t n =
    let d = domains t and per = round_size t n in
    let barrier = Barrier.create d in
    Pool.run t.pool d (work t barrier n);
    t.placed <- true;
    t.rounds <- t.rounds + ((n + per - 1) / per)

  let iterations t = Array.fold_left (fun n w -> n + M.iterations w) 0 t.workers
  let touched t = Array.fold_left (fun n w -> n + M.touched w) 0 t.workers
  let average_strategy t = Table.average_strategy t.master
  let current_strategy t = Table.current_strategy t.master
  let regrets t = Table.regrets t.master
  let strategy_sums t = Table.strategy_sums t.master
end
