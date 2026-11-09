module Make
    (F : Lp.FIELD)
    (T : sig
      val delta : F.t
    end) =
struct
  type t = {
    shift : F.t;
    mutable tab : F.t array array;
    mutable orig : F.t array array;
    mutable rhs : F.t array;
    mutable red : F.t array;
    mutable z : F.t;
    mutable basis : int array;
    mutable slack : int array;
    mutable column : int array;
    mutable m : int;
    mutable n : int;
    mutable vars : int;
    mutable pivots : int;
    mutable refactors : int;
  }

  let zero x = F.sign x = 0
  let nonzero x = F.compare x F.zero <> 0

  let grow a len fill =
    if Array.length a >= len then a
    else
      let b = Array.make (max len (2 * Array.length a)) fill in
      Array.blit a 0 b 0 (Array.length a);
      b

  let create ~shift =
    {
      shift;
      tab = [||];
      orig = [||];
      rhs = [||];
      red = [||];
      z = F.zero;
      basis = [||];
      slack = [||];
      column = [||];
      m = 0;
      n = 0;
      vars = 0;
      pivots = 0;
      refactors = 0;
    }

  let rows t = t.m
  let cols t = t.n
  let pivots t = t.pivots
  let refactors t = t.refactors

  let fresh t =
    let v = t.vars in
    t.vars <- v + 1;
    t.red <- grow t.red t.vars F.zero;
    for i = 0 to t.m - 1 do
      t.tab.(i) <- grow t.tab.(i) t.vars F.zero;
      t.orig.(i) <- grow t.orig.(i) t.vars F.zero
    done;
    v

  let pivot t r s =
    let row = t.tab.(r) in
    let p = row.(s) in
    if F.compare p F.one <> 0 then begin
      for k = 0 to t.vars - 1 do
        if nonzero row.(k) then row.(k) <- F.div row.(k) p
      done;
      t.rhs.(r) <- F.div t.rhs.(r) p
    end;
    row.(s) <- F.one;
    let nz = ref [] in
    for k = t.vars - 1 downto 0 do
      if nonzero row.(k) then nz := k :: !nz
    done;
    let nz = Array.of_list !nz and b = t.rhs.(r) in
    let eliminate other =
      let f = other.(s) in
      if not (nonzero f) then F.zero
      else begin
        Array.iter (fun k -> other.(k) <- F.sub other.(k) (F.mul f row.(k))) nz;
        other.(s) <- F.zero;
        F.mul f b
      end
    in
    for i = 0 to t.m - 1 do
      if i <> r then t.rhs.(i) <- F.sub t.rhs.(i) (eliminate t.tab.(i))
    done;
    t.z <- F.sub t.z (eliminate t.red);
    t.basis.(r) <- s;
    t.pivots <- t.pivots + 1

  let harris ~bland cands =
    let ratio (_, _, num, den) =
      F.div (if F.sign num < 0 then F.zero else num) den
    in
    let limit =
      List.fold_left
        (fun m (_, _, num, den) ->
          let q = F.div (F.add num T.delta) den in
          match m with Some w when F.compare w q <= 0 -> m | _ -> Some q)
        None cands
    in
    let better (key, _, _, den) (key', _, _, den') =
      if bland then key < key' else F.compare den den' > 0
    in
    match limit with
    | None -> None
    | Some limit ->
        List.fold_left
          (fun best c ->
            if F.compare (ratio c) limit > 0 then best
            else
              match best with
              | Some b when not (better c b) -> best
              | _ -> Some c)
          None cands
        |> Option.map (fun ((_, payload, _, _) as c) -> (payload, ratio c))

  let rec primal t stall =
    let bland = stall > 50 in
    let s = ref (-1) in
    for k = 0 to t.vars - 1 do
      if F.sign t.red.(k) < 0 then
        if !s < 0 || ((not bland) && F.compare t.red.(k) t.red.(!s) < 0) then
          s := k
    done;
    if !s >= 0 then begin
      let s = !s and cands = ref [] in
      for i = t.m - 1 downto 0 do
        let a = t.tab.(i).(s) in
        if F.sign a > 0 then cands := (t.basis.(i), i, t.rhs.(i), a) :: !cands
      done;
      match harris ~bland !cands with
      | None -> failwith "Meta_lp: unbounded"
      | Some (r, q) ->
          pivot t r s;
          primal t (if zero q then stall + 1 else 0)
    end

  let rec dual t stall =
    let bland = stall > 50 in
    let r = ref (-1) in
    for i = 0 to t.m - 1 do
      if F.sign t.rhs.(i) < 0 then
        if
          !r < 0
          || (bland && t.basis.(i) < t.basis.(!r))
          || ((not bland) && F.compare t.rhs.(i) t.rhs.(!r) < 0)
        then r := i
    done;
    if !r >= 0 then begin
      let r = !r and cands = ref [] in
      let row = t.tab.(r) in
      for k = t.vars - 1 downto 0 do
        if F.sign row.(k) < 0 then
          cands := (k, k, t.red.(k), F.sub F.zero row.(k)) :: !cands
      done;
      match harris ~bland !cands with
      | None -> failwith "Meta_lp: infeasible"
      | Some (s, q) ->
          pivot t r s;
          dual t (if zero q then stall + 1 else 0)
    end

  let entry t a =
    let x = F.add a t.shift in
    if F.sign x <= 0 then invalid_arg "Meta_lp: shift too small";
    x

  let add_row t a =
    if Array.length a <> t.n then invalid_arg "Meta_lp.add_row: length";
    let v = fresh t in
    let row = Array.make (Array.length t.red) F.zero in
    Array.iteri (fun j x -> row.(t.column.(j)) <- entry t x) a;
    row.(v) <- F.one;
    let raw = Array.copy row in
    let b = ref F.one in
    for i = 0 to t.m - 1 do
      let f = row.(t.basis.(i)) in
      if nonzero f then begin
        let other = t.tab.(i) in
        for k = 0 to t.vars - 1 do
          if nonzero other.(k) then row.(k) <- F.sub row.(k) (F.mul f other.(k))
        done;
        row.(t.basis.(i)) <- F.zero;
        b := F.sub !b (F.mul f t.rhs.(i))
      end
    done;
    let i = t.m in
    t.tab <- grow t.tab (i + 1) [||];
    t.orig <- grow t.orig (i + 1) [||];
    t.orig.(i) <- raw;
    t.rhs <- grow t.rhs (i + 1) F.zero;
    t.basis <- grow t.basis (i + 1) 0;
    t.slack <- grow t.slack (i + 1) 0;
    t.tab.(i) <- row;
    t.rhs.(i) <- !b;
    t.basis.(i) <- v;
    t.slack.(i) <- v;
    t.m <- i + 1;
    dual t 0

  let add_col t a =
    if t.m = 0 then invalid_arg "Meta_lp.add_col: no rows";
    if Array.length a <> t.m then invalid_arg "Meta_lp.add_col: length";
    let a = Array.map (entry t) a in
    let v = fresh t in
    let through row =
      let acc = ref F.zero in
      Array.iteri
        (fun i x ->
          let c = row.(t.slack.(i)) in
          if nonzero c then acc := F.add !acc (F.mul c x))
        a;
      !acc
    in
    for r = 0 to t.m - 1 do
      t.tab.(r).(v) <- through t.tab.(r);
      t.orig.(r).(v) <- a.(r)
    done;
    t.red.(v) <- F.sub (through t.red) F.one;
    t.column <- grow t.column (t.n + 1) 0;
    t.column.(t.n) <- v;
    t.n <- t.n + 1;
    primal t 0

  let solution t =
    if t.m = 0 || t.n = 0 then invalid_arg "Meta_lp.solution: empty game";
    let x = Array.init t.m (fun i -> F.div t.red.(t.slack.(i)) t.z) in
    let y = Array.make t.n F.zero in
    let col = Array.make t.vars (-1) in
    Array.iteri (fun j v -> col.(v) <- j) (Array.sub t.column 0 t.n);
    for i = 0 to t.m - 1 do
      let j = col.(t.basis.(i)) in
      if j >= 0 then y.(j) <- F.div t.rhs.(i) t.z
    done;
    (x, y, F.sub (F.div F.one t.z) t.shift)

  let gap t (x, y) =
    if t.m = 0 || t.n = 0 then invalid_arg "Meta_lp.gap: empty game";
    let dot w f =
      let acc = ref F.zero in
      Array.iteri (fun k p -> acc := F.add !acc (F.mul p (f k))) w;
      !acc
    in
    let a i j = t.orig.(i).(t.column.(j)) in
    let rows = Array.init t.m (fun i -> dot y (a i))
    and cols = Array.init t.n (fun j -> dot x (fun i -> a i j)) in
    let pick better v =
      Array.fold_left (fun m x -> if better x m then x else m) v.(0) v
    in
    F.sub
      (pick (fun x m -> F.compare x m > 0) rows)
      (pick (fun x m -> F.compare x m < 0) cols)

  let magnitude x = if F.sign x < 0 then F.sub F.zero x else x

  let refactor t =
    let target = Array.sub t.basis 0 t.m in
    let wanted = Array.make t.vars false in
    Array.iter (fun v -> wanted.(v) <- true) target;
    for i = 0 to t.m - 1 do
      t.tab.(i) <- Array.copy t.orig.(i);
      t.rhs.(i) <- F.one;
      t.basis.(i) <- t.slack.(i)
    done;
    Array.fill t.red 0 (Array.length t.red) F.zero;
    for j = 0 to t.n - 1 do
      t.red.(t.column.(j)) <- F.sub F.zero F.one
    done;
    t.z <- F.zero;
    Array.iter
      (fun v ->
        if not (Array.exists (( = ) v) (Array.sub t.basis 0 t.m)) then begin
          let best = ref (-1) in
          for i = 0 to t.m - 1 do
            if
              (not wanted.(t.basis.(i)))
              && nonzero t.tab.(i).(v)
              && (!best < 0
                 || F.compare
                      (magnitude t.tab.(i).(v))
                      (magnitude t.tab.(!best).(v))
                    > 0)
            then best := i
          done;
          if !best >= 0 then pivot t !best v
        end)
      target;
    t.refactors <- t.refactors + 1;
    dual t 0;
    primal t 0
end
