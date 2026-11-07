type move = Pass | Bet
type history = Deal | Play of { cards : int array; moves : move list }

let card_name ~players c = String.make 1 "23456789TJQKA".[11 - players + c]
let move_to_string = function Pass -> "p" | Bet -> "b"

let rec deals k pool =
  if k = 0 then [ [] ]
  else
    List.concat_map
      (fun c ->
        List.map (List.cons c) (deals (k - 1) (List.filter (( <> ) c) pool)))
      pool

let first_bet moves =
  let rec go i = function
    | [] -> None
    | Bet :: _ -> Some i
    | Pass :: rest -> go (i + 1) rest
  in
  go 0 moves

let settle n cards moves bettor =
  let paid = Array.make n 1 and live = Array.make n (bettor = None) in
  List.iteri
    (fun j m ->
      if m = Bet then begin
        paid.(j mod n) <- 2;
        live.(j mod n) <- true
      end)
    moves;
  let winner = ref (-1) in
  Array.iteri
    (fun i alive ->
      if alive && (!winner < 0 || cards.(i) > cards.(!winner)) then winner := i)
    live;
  let pot = Array.fold_left ( + ) 0 paid in
  Array.init n (fun i ->
      float_of_int ((if i = !winner then pot else 0) - paid.(i)))

let game n =
  if n < 2 || n > 11 then invalid_arg "Kuhn_n.game: players must be in [2, 11]";
  let module G = struct
    type nonrec history = history

    let name = Printf.sprintf "kuhn%d" n
    let players = n
    let root = Deal
    let all = List.map Array.of_list (deals n (List.init (n + 1) Fun.id))
    let p = 1. /. float_of_int (List.length all)

    let node = function
      | Deal ->
          Multi.Chance
            (List.map (fun cards -> (p, Play { cards; moves = [] })) all)
      | Play { cards; moves } -> (
          let len = List.length moves and bettor = first_bet moves in
          match bettor with
          | None when len = n -> Multi.Terminal (settle n cards moves bettor)
          | Some b when len = b + n -> Terminal (settle n cards moves bettor)
          | _ ->
              let player = len mod n in
              let infoset =
                card_name ~players:n cards.(player)
                ^ String.concat "" (List.map move_to_string moves)
              in
              let child m =
                (move_to_string m, Play { cards; moves = moves @ [ m ] })
              in
              Decision { player; infoset; actions = [ child Pass; child Bet ] })
  end in
  (module G : Multi.S with type history = history)
