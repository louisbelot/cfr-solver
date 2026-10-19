type card = J | Q | K
type move = Pass | Bet

(* [moves] is in the order played; a Kuhn hand is at most three moves long, so
   appending is cheap. *)
type history = Deal | Play of { cards : card * card; moves : move list }

let name = "kuhn"
let root = Deal
let rank = function J -> 0 | Q -> 1 | K -> 2
let card_to_string = function J -> "J" | Q -> "Q" | K -> "K"
let move_to_string = function Pass -> "p" | Bet -> "b"
let deals = [ (J, Q); (J, K); (Q, J); (Q, K); (K, J); (K, Q) ]

(* Both players ante 1, a bet is 1, so the winner of a called bet takes 2. *)
let showdown (c0, c1) stake = if rank c0 > rank c1 then stake else -.stake

let node = function
  | Deal ->
      Game.Chance
        (List.map (fun cards -> (1. /. 6., Play { cards; moves = [] })) deals)
  | Play { cards; moves } -> (
      match moves with
      | [ Pass; Pass ] -> Game.Terminal (showdown cards 1.)
      | [ Bet; Pass ] -> Terminal 1.
      | [ Pass; Bet; Pass ] -> Terminal (-1.)
      | [ Bet; Bet ] | [ Pass; Bet; Bet ] -> Terminal (showdown cards 2.)
      | _ ->
          let player = if List.length moves mod 2 = 0 then Game.P0 else P1 in
          let card = match player with P0 -> fst cards | P1 -> snd cards in
          let infoset =
            card_to_string card
            ^ String.concat "" (List.map move_to_string moves)
          in
          let child m =
            (move_to_string m, Play { cards; moves = moves @ [ m ] })
          in
          Decision { player; infoset; actions = [ child Pass; child Bet ] })
