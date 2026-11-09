type order = Descending | Ascending | Random
type payoff = Win_loss | Points
type config = { cards : int; order : order; payoff : payoff; reveal : bool }

let standard =
  { cards = 4; order = Descending; payoff = Win_loss; reveal = false }

let check c =
  if c.cards < 1 || c.cards > 13 then
    invalid_arg "Goofspiel: need 1 <= cards <= 13"

let name c =
  Printf.sprintf "goofspiel-%d%s%s%s" c.cards
    (match c.order with
    | Descending -> ""
    | Ascending -> "-asc"
    | Random -> "-random")
    (if c.reveal then "-open" else "")
    (match c.payoff with Win_loss -> "" | Points -> "-points")

let full c = (1 lsl c.cards) - 1
let bit k = 1 lsl (k - 1)
let mask = List.fold_left (fun m k -> m lor bit k) 0

let members m =
  let rec go k acc =
    if k = 0 then acc
    else go (k - 1) (if m land bit k <> 0 then k :: acc else acc)
  in
  go 13 []

let next c deck =
  match (c.order, members (full c land lnot deck)) with
  | Random, _ | _, [] -> None
  | Ascending, k :: _ -> Some k
  | Descending, left -> Some (List.hd (List.rev left))

type history = {
  deck : int list;
  b0 : int list;
  b1 : int list;
  pending : int option;
}

let deal c h =
  if List.length h.b0 >= c.cards - 1 then h
  else
    match next c (mask h.deck) with
    | Some k -> { h with deck = k :: h.deck }
    | None -> h

let root c =
  check c;
  deal c { deck = []; b0 = []; b1 = []; pending = None }

let rec score deck b0 b1 acc =
  match (deck, b0, b1) with
  | k :: d, x :: r0, y :: r1 -> score d r0 r1 (acc + (k * compare x y))
  | _ -> acc

let last c l = members (full c land lnot (mask l)) |> List.hd

let finish c h =
  let deck = last c h.deck :: h.deck
  and b0 = last c h.b0 :: h.b0
  and b1 = last c h.b1 :: h.b1 in
  let s = score deck b0 b1 0 in
  match c.payoff with Points -> float s | Win_loss -> float (compare s 0)

let ints l = String.concat "," (List.rev_map string_of_int l)

let outcomes own opp =
  String.concat ""
    (List.rev
       (List.map2
          (fun x y -> if x > y then "W" else if x < y then "L" else "T")
          own opp))

let infoset c player h =
  let own, opp =
    match player with Game.P0 -> (h.b0, h.b1) | P1 -> (h.b1, h.b0)
  in
  Printf.sprintf "%d|%s|%s|%s"
    (match player with Game.P0 -> 0 | P1 -> 1)
    (ints h.deck) (ints own)
    (if c.reveal then ints opp else outcomes own opp)

let hand c bids = members (full c land lnot (mask bids))

let node c h =
  match h.pending with
  | Some x ->
      Game.Decision
        {
          player = P1;
          infoset = infoset c P1 h;
          actions =
            List.map
              (fun y ->
                ( string_of_int y,
                  deal c
                    { h with b0 = x :: h.b0; b1 = y :: h.b1; pending = None } ))
              (hand c h.b1);
        }
  | None ->
      if List.length h.b0 >= c.cards - 1 then Game.Terminal (finish c h)
      else if List.length h.deck = List.length h.b0 then
        let left = hand c h.deck in
        let p = 1. /. float (List.length left) in
        Chance (List.map (fun k -> (p, { h with deck = k :: h.deck })) left)
      else
        Decision
          {
            player = P0;
            infoset = infoset c P0 h;
            actions =
              List.map
                (fun x -> (string_of_int x, { h with pending = Some x }))
                (hand c h.b0);
          }

module Make (C : sig
  val config : config
end) =
struct
  type nonrec history = history

  let name = name C.config
  let root = root C.config
  let node = node C.config
end

let game config =
  (module Make (struct
    let config = config
  end) : Game.S)

let profile c solve =
  if not c.reveal then invalid_arg "Goofspiel.profile: needs revealed bids";
  let all = full c in
  let table = Hashtbl.create 1024 in
  let rec walk h =
    match node c h with
    | Game.Terminal _ -> ()
    | Chance outcomes -> List.iter (fun (_, h') -> walk h') outcomes
    | Decision { player; infoset; actions } ->
        if not (Hashtbl.mem table infoset) then begin
          let mine = all land lnot (mask h.b0)
          and theirs = all land lnot (mask h.b1)
          and deck = all land lnot (mask (List.tl h.deck)) in
          let x, y = solve ~mine ~theirs ~deck ~upcard:(List.hd h.deck) in
          let probs = match player with P0 -> x | P1 -> y in
          Hashtbl.replace table infoset
            (Array.of_list (List.map fst actions), probs)
        end;
        List.iter (fun (_, h') -> walk h') actions
  in
  walk (root c);
  Hashtbl.fold
    (fun key (actions, probs) s -> Strategy.add key ~actions ~probs s)
    table Strategy.empty
