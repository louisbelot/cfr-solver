(* Declared in rank order, so [compare] orders cards. *)
type rank = J | Q | K
type action = Fold | Call | Raise

(* Betting lists are in the order played. A round has at most four actions. *)
type history =
  | Start
  | Dealt of rank
  | Round1 of { hole : rank * rank; bets : action list }
  | Round2 of {
      hole : rank * rank;
      board : rank;
      first : action list;
      bets : action list;
    }

let name = "leduc"
let root = Start
let ranks = [ J; Q; K ]
let rank_to_string = function J -> "J" | Q -> "Q" | K -> "K"
let action_to_string = function Fold -> "f" | Call -> "c" | Raise -> "r"
let actions_to_string bets = String.concat "" (List.map action_to_string bets)
let player_of i = if i mod 2 = 0 then Game.P0 else P1

(* Draw one of the remaining cards. *)
let deal dealt k =
  let left = 6 - List.length dealt in
  List.filter_map
    (fun r ->
      match 2 - List.length (List.filter (( = ) r) dealt) with
      | 0 -> None
      | n -> Some (float_of_int n /. float_of_int left, k r))
    ranks

type status = Acting of Game.player | Closed | Folded of Game.player

(* A round closes on a call that is not the opening check: check-check, or a
   call of a raise. *)
let status bets =
  let n = List.length bets in
  match List.rev bets with
  | Fold :: _ -> Folded (player_of (n - 1))
  | Call :: _ when n >= 2 -> Closed
  | _ -> Acting (player_of n)

(* Chips each player puts in during one round. A call matches the highest
   contribution so far; a raise matches it and adds [size]. *)
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

(* Player 0's payoff: what the loser put in, the ante included. *)
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

let node = function
  | Start -> Game.Chance (deal [] (fun r -> Dealt r))
  | Dealt c0 ->
      Chance (deal [ c0 ] (fun c1 -> Round1 { hole = (c0, c1); bets = [] }))
  | Round1 { hole; bets } -> (
      match status bets with
      | Folded p -> Terminal (payoff hole bets [] (`Fold p))
      | Closed ->
          let c0, c1 = hole in
          Chance
            (deal [ c0; c1 ] (fun board ->
                 Round2 { hole; board; first = bets; bets = [] }))
      | Acting p ->
          decision p
            (rank_to_string (card p hole) ^ ":" ^ actions_to_string bets)
            bets
            (fun bets -> Round1 { hole; bets }))
  | Round2 { hole; board; first; bets } -> (
      match status bets with
      | Folded p -> Terminal (payoff hole first bets (`Fold p))
      | Closed -> Terminal (payoff hole first bets (`Showdown board))
      | Acting p ->
          decision p
            (rank_to_string (card p hole)
            ^ rank_to_string board ^ ":" ^ actions_to_string first ^ "/"
            ^ actions_to_string bets)
            bets
            (fun bets -> Round2 { hole; board; first; bets }))
