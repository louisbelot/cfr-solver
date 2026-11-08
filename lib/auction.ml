type rule = First_price | Second_price | All_pay
type ties = Split | Void

type config = {
  bidders : int;
  top : int;
  rule : rule;
  ties : ties;
  reserve : int;
}

let check c =
  if c.bidders < 2 then invalid_arg "Auction: need at least two bidders";
  if c.top < 1 then invalid_arg "Auction: need top >= 1";
  if c.reserve < 0 || c.reserve > c.top then
    invalid_arg "Auction: reserve must be in [0, top]"

let rule_name = function
  | First_price -> "first"
  | Second_price -> "second"
  | All_pay -> "allpay"

let name c =
  Printf.sprintf "%s-n%d-x%d%s%s" (rule_name c.rule) c.bidders c.top
    (match c.ties with Split -> "" | Void -> "-void")
    (if c.reserve > 0 then Printf.sprintf "-r%d" c.reserve else "")

let key i v = Printf.sprintf "%d:%d" i v

module type FIELD = sig
  type t

  val zero : t
  val one : t
  val of_int : int -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val compare : t -> t -> int
end

module type EVAL = sig
  type num
  type profile = num array array array

  val pure : config -> int array array -> profile
  val symmetric : config -> int array -> profile
  val outcome : config -> num array array -> num array * num array
  val utilities : config -> profile -> int -> num array array
  val value : config -> profile -> int -> num
  val best : config -> profile -> int -> num * int array
  val gains : config -> profile -> num array
  val nash_conv : config -> profile -> num
  val revenue : config -> profile -> num
  val is_equilibrium : config -> profile -> bool
  val symmetric_pure_equilibria : config -> int array list
end

module Make (F : FIELD) = struct
  type num = F.t
  type profile = num array array array

  let ( +: ) = F.add
  let ( -: ) = F.sub
  let ( *: ) = F.mul
  let prior c = F.div F.one (F.of_int (c.top + 1))

  let pure c bids =
    check c;
    Array.map
      (fun row ->
        Array.map
          (fun b ->
            Array.init (c.top + 1) (fun k -> if k = b then F.one else F.zero))
          row)
      bids

  let symmetric c beta = pure c (Array.make c.bidders beta)

  let marginal c s =
    let p = prior c in
    let q = Array.make (c.top + 1) F.zero in
    Array.iter (Array.iteri (fun b x -> q.(b) <- q.(b) +: (p *: x))) s;
    q

  let outcome c others =
    check c;
    let m = c.top + 1 in
    let k = Array.length others in
    let lt = Array.make k F.zero in
    let win = Array.make m F.zero and pay = Array.make m F.zero in
    let below = ref F.zero and seen = ref F.zero in
    for b = 0 to m - 1 do
      let poly = Array.make (k + 1) F.zero in
      poly.(0) <- F.one;
      Array.iteri
        (fun j q ->
          for t = j + 1 downto 0 do
            let tie = if t > 0 then poly.(t - 1) *: q.(b) else F.zero in
            poly.(t) <- (poly.(t) *: lt.(j)) +: tie
          done)
        others;
      let shared = ref F.zero in
      for t = 1 to k do
        shared := !shared +: F.div poly.(t) (F.of_int (t + 1))
      done;
      let le = ref F.one in
      Array.iteri (fun j q -> le := !le *: (lt.(j) +: q.(b))) others;
      if b >= c.reserve then begin
        let w =
          match c.ties with Split -> poly.(0) +: !shared | Void -> poly.(0)
        in
        win.(b) <- w;
        pay.(b) <-
          (match c.rule with
          | First_price -> F.of_int b *: w
          | All_pay -> F.of_int b
          | Second_price -> (
              match c.ties with
              | Split -> !below +: (F.of_int b *: !shared)
              | Void -> !below))
      end;
      below := !below +: ((!le -: !seen) *: F.of_int (max c.reserve b));
      seen := !le;
      Array.iteri (fun j q -> lt.(j) <- lt.(j) +: q.(b)) others
    done;
    (win, pay)

  let rivals c s i =
    List.init c.bidders Fun.id
    |> List.filter (( <> ) i)
    |> List.map (fun j -> marginal c s.(j))
    |> Array.of_list

  let utilities c s i =
    let win, pay = outcome c (rivals c s i) in
    Array.init (c.top + 1) (fun v ->
        Array.init (c.top + 1) (fun b -> (F.of_int v *: win.(b)) -: pay.(b)))

  let expect row u =
    let acc = ref F.zero in
    Array.iteri (fun b x -> acc := !acc +: (x *: u.(b))) row;
    !acc

  let value c s i =
    let u = utilities c s i and p = prior c in
    let acc = ref F.zero in
    Array.iteri (fun v row -> acc := !acc +: (p *: expect row u.(v))) s.(i);
    !acc

  let best c s i =
    let u = utilities c s i and p = prior c in
    let acc = ref F.zero in
    let bids =
      Array.map
        (fun row ->
          let arg = ref 0 in
          Array.iteri
            (fun b x -> if F.compare x row.(!arg) > 0 then arg := b)
            row;
          acc := !acc +: (p *: row.(!arg));
          !arg)
        u
    in
    (!acc, bids)

  let gains c s =
    Array.init c.bidders (fun i -> fst (best c s i) -: value c s i)

  let nash_conv c s = Array.fold_left ( +: ) F.zero (gains c s)

  let revenue c s =
    let p = prior c in
    let acc = ref F.zero in
    for i = 0 to c.bidders - 1 do
      let _, pay = outcome c (rivals c s i) in
      Array.iter (fun row -> acc := !acc +: (p *: expect row pay)) s.(i)
    done;
    !acc

  let is_equilibrium c s =
    Array.for_all (fun g -> F.compare g F.zero <= 0) (gains c s)

  let symmetric_pure_equilibria c =
    check c;
    let m = c.top + 1 in
    let beta = Array.make m 0 and found = ref [] in
    let rec fill v =
      if v = m then begin
        let s = symmetric c beta in
        let g = fst (best c s 0) -: value c s 0 in
        if F.compare g F.zero <= 0 then found := Array.copy beta :: !found
      end
      else
        for b = 0 to v do
          beta.(v) <- b;
          fill (v + 1)
        done
    in
    fill 0;
    List.rev !found
end

module Approx = Make (struct
  type t = float

  let zero = 0.
  let one = 1.
  let of_int = float_of_int
  let add = ( +. )
  let sub = ( -. )
  let mul = ( *. )
  let div = ( /. )
  let compare = Float.compare
end)

module Exact = Make (Rat)

let rationalize ?(max_den = 1000) s =
  let near x =
    try Rat.recover ~max_den x with Invalid_argument _ -> Rat.of_float x
  in
  let row probs =
    let q = Array.map near probs in
    let total = Array.fold_left Rat.add Rat.zero q in
    Array.map (fun x -> Rat.div x total) q
  in
  Array.map (Array.map row) s

let strategy c s =
  let actions = Array.init (c.top + 1) string_of_int in
  let out = ref Strategy.empty in
  Array.iteri
    (fun i rows ->
      Array.iteri
        (fun v probs -> out := Strategy.add (key i v) ~actions ~probs !out)
        rows)
    s;
  !out

let profile c strat =
  check c;
  Array.init c.bidders (fun i ->
      Array.init (c.top + 1) (fun v ->
          Strategy.probs strat (key i v) ~n:(c.top + 1)))

let settle c values bids =
  let n = c.bidders in
  let hi = Array.fold_left max (-1) bids in
  let winners =
    if hi < c.reserve then []
    else List.filter (fun i -> bids.(i) = hi) (List.init n Fun.id)
  in
  let winners =
    match (c.ties, winners) with Void, _ :: _ :: _ -> [] | _ -> winners
  in
  let share = 1. /. float_of_int (max 1 (List.length winners)) in
  let price =
    match winners with
    | [] -> 0
    | [ w ] when c.rule = Second_price ->
        Array.to_list bids
        |> List.filteri (fun j _ -> j <> w)
        |> List.fold_left max c.reserve
    | _ -> hi
  in
  Array.init n (fun i ->
      let won = List.mem i winners in
      let gain = if won then share *. float_of_int values.(i) else 0. in
      let paid =
        match c.rule with
        | All_pay -> if bids.(i) >= c.reserve then float_of_int bids.(i) else 0.
        | First_price | Second_price ->
            if won then share *. float_of_int price else 0.
      in
      gain -. paid)

type history = Deal | Bids of { values : int array; bids : int list }

let game c =
  check c;
  let module G = struct
    type nonrec history = history

    let name = name c
    let players = c.bidders
    let root = Deal
    let m = c.top + 1

    let rec profiles k =
      if k = 0 then [ [] ]
      else
        List.concat_map
          (fun rest -> List.init m (fun v -> v :: rest))
          (profiles (k - 1))

    let deals =
      let p = 1. /. Float.pow (float_of_int m) (float_of_int c.bidders) in
      List.map
        (fun vs -> (p, Bids { values = Array.of_list vs; bids = [] }))
        (profiles c.bidders)

    let node = function
      | Deal -> Multi.Chance deals
      | Bids { values; bids } when List.length bids = c.bidders ->
          Terminal (settle c values (Array.of_list (List.rev bids)))
      | Bids { values; bids } ->
          let i = List.length bids in
          Decision
            {
              player = i;
              infoset = key i values.(i);
              actions =
                List.init m (fun b ->
                    (string_of_int b, Bids { values; bids = b :: bids }));
            }
  end in
  (module G : Multi.S with type history = history)

module Solver = struct
  type t = {
    config : config;
    algorithm : Cfr.algorithm;
    symmetric : bool;
    regret : float array array array;
    sum : float array array array;
    cur : float array array array;
    mutable iterations : int;
  }

  let create ?(algorithm = Cfr.Vanilla) ?(symmetric = false) c =
    check c;
    if Cfr.predictive algorithm then
      invalid_arg "Auction.Solver.create: predictive updates are not supported";
    let tables = if symmetric then 1 else c.bidders in
    let grid () =
      Array.init tables (fun _ ->
          Array.init (c.top + 1) (fun _ -> Array.make (c.top + 1) 0.))
    in
    let uniform = 1. /. float_of_int (c.top + 1) in
    {
      config = c;
      algorithm;
      symmetric;
      regret = grid ();
      sum = grid ();
      cur = Array.map (Array.map (Array.map (fun _ -> uniform))) (grid ());
      iterations = 0;
    }

  let iterations t = t.iterations
  let expand t a = if t.symmetric then Array.make t.config.bidders a.(0) else a
  let current t = expand t (Array.map (Array.map Array.copy) t.cur)

  let average t =
    expand t (Array.map (Array.map Regret_matching.normalize) t.sum)

  let marginal p rows =
    let m = Array.length rows in
    let q = Array.make m 0. in
    for v = 0 to m - 1 do
      let row = rows.(v) in
      for b = 0 to m - 1 do
        q.(b) <- q.(b) +. (p *. row.(b))
      done
    done;
    q

  let update t s i ~pos ~neg ~w =
    let c = t.config in
    let m = c.top + 1 in
    let p = 1. /. float_of_int m in
    let others =
      Array.init (c.bidders - 1) (fun j ->
          marginal p s.(if j < i then j else j + 1))
    in
    let win, pay = Approx.outcome c others in
    let u = Array.make m 0. in
    for v = 0 to m - 1 do
      let row = s.(i).(v) and r = t.regret.(i).(v) and sum = t.sum.(i).(v) in
      let fv = float_of_int v in
      let ev = ref 0. in
      for b = 0 to m - 1 do
        u.(b) <- (fv *. win.(b)) -. pay.(b);
        ev := !ev +. (row.(b) *. u.(b))
      done;
      for b = 0 to m - 1 do
        let x = r.(b) +. (p *. (u.(b) -. !ev)) in
        r.(b) <- (x *. if x > 0. then pos else neg);
        sum.(b) <- sum.(b) +. (w *. row.(b))
      done
    done

  let pass t updating n w =
    let s = expand t t.cur in
    let pos, neg, _ = Cfr.discounts t.algorithm n in
    for i = 0 to Array.length t.regret - 1 do
      if updating i then update t s i ~pos ~neg ~w
    done;
    for i = 0 to Array.length t.regret - 1 do
      if updating i then
        Array.iteri
          (fun v r -> Regret_matching.strategy_into r t.cur.(i).(v))
          t.regret.(i)
    done

  let iterate t =
    let n = t.iterations + 1 in
    let w = Cfr.weight t.algorithm n in
    if Cfr.alternating t.algorithm && not t.symmetric then
      for i = 0 to t.config.bidders - 1 do
        pass t (( = ) i) n w
      done
    else pass t (fun _ -> true) n w;
    t.iterations <- n

  let run t k =
    for _ = 1 to k do
      iterate t
    done
end

module Theory = struct
  let bid rule ~bidders ?(reserve = 0.) v =
    let n = float_of_int bidders in
    match rule with
    | Second_price -> v
    | First_price ->
        if v < reserve then Float.nan
        else if v = 0. then 0.
        else
          v
          -. (Float.pow v n -. Float.pow reserve n)
             /. (n *. Float.pow v (n -. 1.))
    | All_pay ->
        if reserve > 0. then
          invalid_arg "Auction.Theory.bid: all-pay with a reserve";
        (n -. 1.) /. n *. Float.pow v n

  let revenue ~bidders ?(reserve = 0.) () =
    let n = float_of_int bidders in
    (2. *. n /. (n +. 1.) *. (1. -. Float.pow reserve (n +. 1.)))
    -. (1. -. Float.pow reserve n)
end
