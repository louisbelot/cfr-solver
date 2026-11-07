type action = Fold | Call | Raise

type history =
  | Dealing of int list
  | Round1 of { hole : int array; bets : action list }
  | Round2 of {
      hole : int array;
      board : int;
      first : action list;
      bets : action list;
    }

type table = {
  put : int array;
  out : bool array;
  mutable level : int;
  mutable raises : int;
  mutable calls : int;
  mutable turn : int;
}

let label = function Fold -> "f" | Call -> "c" | Raise -> "r"
let labels bets = String.concat "" (List.map label bets)
let left t = Array.fold_left (fun k o -> if o then k else k + 1) 0 t.out

let next t p =
  let n = Array.length t.out in
  let rec go q = if t.out.(q) then go ((q + 1) mod n) else q in
  go ((p + 1) mod n)

let start put out level =
  let t =
    {
      put = Array.copy put;
      out = Array.copy out;
      level;
      raises = 0;
      calls = 0;
      turn = 0;
    }
  in
  if t.out.(0) then t.turn <- next t 0;
  t

let act size t a =
  let p = t.turn in
  (match a with
  | Fold -> t.out.(p) <- true
  | Call ->
      t.put.(p) <- t.level;
      t.calls <- t.calls + 1
  | Raise ->
      t.level <- t.level + size;
      t.put.(p) <- t.level;
      t.raises <- t.raises + 1;
      t.calls <- 0);
  t.turn <- next t p

let replay size t bets =
  List.iter (act size t) bets;
  t

let closed t =
  let k = left t in
  k = 1 || (t.raises = 0 && t.calls = k) || (t.raises > 0 && t.calls = k - 1)

let settle t winners =
  let pot = float_of_int (Array.fold_left ( + ) 0 t.put) in
  let share = pot /. float_of_int (List.length winners) in
  Array.mapi
    (fun i put ->
      (if List.mem i winners then share else 0.) -. float_of_int put)
    t.put

let showdown t hole board =
  let score i = ((if hole.(i) = board then 1 else 0), hole.(i)) in
  let live =
    List.filter (fun i -> not t.out.(i)) (List.init (Array.length hole) Fun.id)
  in
  let best =
    List.fold_left (fun b i -> max b (score i)) (score (List.hd live)) live
  in
  settle t (List.filter (fun i -> score i = best) live)

let only t =
  List.filter (fun i -> not t.out.(i)) (List.init (Array.length t.out) Fun.id)

let game ?ranks n =
  if n < 2 || n > 10 then invalid_arg "Leduc_n.game: players must be in [2, 10]";
  let ranks = Option.value ranks ~default:(n + 1) in
  if 2 * ranks < n + 1 || ranks > 13 then
    invalid_arg "Leduc_n.game: deck too small";
  let d = { Leduc.ranks; suits = 2 } in
  let deal dealt k =
    let remaining = Leduc.cards d - List.length dealt in
    List.init d.ranks Fun.id
    |> List.filter_map (fun r ->
        match d.suits - List.length (List.filter (( = ) r) dealt) with
        | 0 -> None
        | s -> Some (float_of_int s /. float_of_int remaining, k r))
  in
  let rank r = Leduc.rank_name d r in
  let ante = Array.make n 1 and nobody = Array.make n false in
  let decision t key hole bets =
    let p = t.turn in
    let legal =
      (if t.level > t.put.(p) then [ Fold ] else [])
      @ [ Call ]
      @ if t.raises < 2 then [ Raise ] else []
    in
    Multi.Decision
      {
        player = p;
        infoset = rank hole.(p) ^ key;
        actions = List.map (fun a -> (label a, bets a)) legal;
      }
  in
  let module G = struct
    type nonrec history = history

    let name =
      if ranks = n + 1 then Printf.sprintf "leduc-%dp" n
      else Printf.sprintf "leduc-%dp-%dr" n ranks

    let players = n
    let root = Dealing []

    let deal_next dealt r =
      let dealt = r :: dealt in
      if List.length dealt = n then
        Round1 { hole = Array.of_list (List.rev dealt); bets = [] }
      else Dealing dealt

    let node = function
      | Dealing dealt -> Multi.Chance (deal dealt (deal_next dealt))
      | Round1 { hole; bets } ->
          let t = replay 2 (start ante nobody 1) bets in
          if left t = 1 then Multi.Terminal (settle t (only t))
          else if closed t then
            Chance
              (deal (Array.to_list hole) (fun board ->
                   Round2 { hole; board; first = bets; bets = [] }))
          else
            decision t
              (":" ^ labels bets)
              hole
              (fun a -> Round1 { hole; bets = bets @ [ a ] })
      | Round2 { hole; board; first; bets } ->
          let t1 = replay 2 (start ante nobody 1) first in
          let t = replay 4 (start t1.put t1.out t1.level) bets in
          if left t = 1 then Multi.Terminal (settle t (only t))
          else if closed t then Terminal (showdown t hole board)
          else
            decision t
              (rank board ^ ":" ^ labels first ^ "/" ^ labels bets)
              hole
              (fun a -> Round2 { hole; board; first; bets = bets @ [ a ] })
  end in
  (module G : Multi.S with type history = history)
