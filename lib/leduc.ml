type deck = { ranks : int; suits : int }

let standard = { ranks = 3; suits = 2 }
let cards d = d.ranks * d.suits

let rank_name d r =
  if d.ranks = 13 then String.make 1 "23456789TJQKA".[r]
  else String.make 1 "23456789TJQK".[12 - d.ranks + r]

type abstraction = {
  name : string;
  round1 : int -> string;
  round2 : int -> int -> string;
}

let exact d =
  {
    name = "exact";
    round1 = rank_name d;
    round2 = (fun c b -> rank_name d c ^ rank_name d b);
  }

type action = Fold | Call | Raise

type history =
  | Start
  | Dealt of int
  | Round1 of { hole : int * int; bets : action list }
  | Round2 of {
      hole : int * int;
      board : int;
      first : action list;
      bets : action list;
    }

let action_to_string = function Fold -> "f" | Call -> "c" | Raise -> "r"
let actions_to_string bets = String.concat "" (List.map action_to_string bets)
let player_of i = if i mod 2 = 0 then Game.P0 else P1

let deal d dealt k =
  let left = cards d - List.length dealt in
  List.init d.ranks Fun.id
  |> List.filter_map (fun r ->
      match d.suits - List.length (List.filter (( = ) r) dealt) with
      | 0 -> None
      | n -> Some (float_of_int n /. float_of_int left, k r))

type status = Acting of Game.player | Closed | Folded of Game.player

let status bets =
  let n = List.length bets in
  match List.rev bets with
  | Fold :: _ -> Folded (player_of (n - 1))
  | Call :: _ when n >= 2 -> Closed
  | _ -> Acting (player_of n)

let committed size bets =
  let c = [| 0; 0 |] in
  List.iteri
    (fun i a ->
      let p = i mod 2 and level = max c.(0) c.(1) in
      match a with
      | Fold -> ()
      | Call -> c.(p) <- level
      | Raise -> c.(p) <- level + size)
    bets;
  c

let pairs board r = if r = board then 1 else 0

let compare_hands board (c0, c1) =
  match compare (pairs board c0) (pairs board c1) with
  | 0 -> compare c0 c1
  | c -> c

let payoff hole first second outcome =
  let c1 = committed 2 first and c2 = committed 4 second in
  let put p = 1 + c1.(p) + c2.(p) in
  float_of_int
    (match outcome with
    | `Fold Game.P0 -> -put 0
    | `Fold P1 -> put 1
    | `Showdown board -> (
        match compare_hands board hole with
        | 0 -> 0
        | c when c > 0 -> put 1
        | _ -> -put 0))

let decision player key bets k =
  let facing = match List.rev bets with Raise :: _ -> true | _ -> false in
  let raises = List.length (List.filter (( = ) Raise) bets) in
  let legal =
    (if facing then [ Fold ] else [])
    @ [ Call ]
    @ if raises < 2 then [ Raise ] else []
  in
  Game.Decision
    {
      player;
      infoset = key;
      actions = List.map (fun a -> (action_to_string a, k (bets @ [ a ]))) legal;
    }

let card player (c0, c1) = match player with Game.P0 -> c0 | P1 -> c1

let node d a = function
  | Start -> Game.Chance (deal d [] (fun r -> Dealt r))
  | Dealt c0 ->
      Chance (deal d [ c0 ] (fun c1 -> Round1 { hole = (c0, c1); bets = [] }))
  | Round1 { hole; bets } -> (
      match status bets with
      | Folded p -> Terminal (payoff hole bets [] (`Fold p))
      | Closed ->
          let c0, c1 = hole in
          Chance
            (deal d [ c0; c1 ] (fun board ->
                 Round2 { hole; board; first = bets; bets = [] }))
      | Acting p ->
          decision p
            (a.round1 (card p hole) ^ ":" ^ actions_to_string bets)
            bets
            (fun bets -> Round1 { hole; bets }))
  | Round2 { hole; board; first; bets } -> (
      match status bets with
      | Folded p -> Terminal (payoff hole first bets (`Fold p))
      | Closed -> Terminal (payoff hole first bets (`Showdown board))
      | Acting p ->
          decision p
            (a.round2 (card p hole) board
            ^ ":" ^ actions_to_string first ^ "/" ^ actions_to_string bets)
            bets
            (fun bets -> Round2 { hole; board; first; bets }))

let deck_name d =
  if d = standard then "leduc"
  else if d.suits = 2 then Printf.sprintf "leduc%d" d.ranks
  else Printf.sprintf "leduc%dx%d" d.ranks d.suits

let game ?abstraction d =
  if d.ranks < 1 || d.ranks > 13 then invalid_arg "Leduc.game: 1 to 13 ranks";
  if d.suits < 1 || cards d < 3 then invalid_arg "Leduc.game: too few cards";
  let a = Option.value abstraction ~default:(exact d) in
  let module G = struct
    type nonrec history = history

    let name =
      match abstraction with
      | None -> deck_name d
      | Some a -> deck_name d ^ "/" ^ a.name

    let root = Start
    let node = node d a
  end in
  (module G : Game.S with type history = history)

module Standard = (val game standard)

let name = Standard.name
let root = Standard.root
let node = Standard.node
