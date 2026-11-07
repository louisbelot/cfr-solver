type config = { dice : int; faces : int; wild : bool }

let standard = { dice = 1; faces = 6; wild = true }

let check c =
  if c.dice < 1 || c.faces < 2 || c.faces > 9 then
    invalid_arg "Liars_dice: need dice >= 1 and 2 <= faces <= 9"

let name c =
  Printf.sprintf "liars-%dd%d%s" c.dice c.faces
    (if c.wild then "" else "-nowild")

let bids c = 2 * c.dice * c.faces
let quantity c b = (b / c.faces) + 1
let face c b = (b mod c.faces) + 1
let bid_to_string c b = Printf.sprintf "%dx%d" (quantity c b) (face c b)
let rec fact n = if n <= 1 then 1. else float n *. fact (n - 1)

let rec multisets n lo hi =
  if n = 0 then [ [] ]
  else
    List.init (hi - lo + 1) (fun i -> lo + i)
    |> List.concat_map (fun f ->
        List.map (fun r -> f :: r) (multisets (n - 1) f hi))

let rec runs = function
  | [] -> []
  | x :: rest -> (
      match runs rest with
      | (y, k) :: tl when x = y -> (y, k + 1) :: tl
      | tl -> (x, 1) :: tl)

let rolls c =
  check c;
  let total = Float.pow (float c.faces) (float c.dice) in
  multisets c.dice 1 c.faces
  |> List.map (fun r ->
      let ways =
        List.fold_left (fun w (_, k) -> w /. fact k) (fact c.dice) (runs r)
      in
      (ways /. total, r))

let holds c r0 r1 b =
  let f = face c b in
  let matches d = d = f || (c.wild && f <> c.faces && d = c.faces) in
  let count r = List.length (List.filter matches r) in
  count r0 + count r1 >= quantity c b

module Make (C : sig
  val config : config
end) =
struct
  let c = C.config
  let () = check c
  let nbids = bids c

  type history =
    | Deal
    | Play of { rolls : int list * int list; bids : int list; called : bool }

  let name = name c
  let root = Deal
  let roll_to_string r = String.concat "" (List.map string_of_int r)
  let player_of n = if n mod 2 = 0 then Game.P0 else P1

  let deals =
    let rs = rolls c in
    List.concat_map
      (fun (p0, r0) ->
        List.map
          (fun (p1, r1) ->
            (p0 *. p1, Play { rolls = (r0, r1); bids = []; called = false }))
          rs)
      rs

  let node = function
    | Deal -> Game.Chance deals
    | Play { rolls = r0, r1; bids = last :: _ as bids; called = true } ->
        let bidder = player_of (List.length bids - 1) in
        let u = if holds c r0 r1 last then 1. else -1. in
        Game.Terminal (Game.sign bidder *. u)
    | Play { rolls; bids; _ } ->
        let n = List.length bids in
        let player = player_of n in
        let own = match player with P0 -> fst rolls | P1 -> snd rolls in
        let infoset =
          roll_to_string own ^ ":"
          ^ String.concat "," (List.rev_map (bid_to_string c) bids)
        in
        let next = match bids with [] -> 0 | b :: _ -> b + 1 in
        let raises =
          List.init (nbids - next) (fun i ->
              let b = next + i in
              ( bid_to_string c b,
                Play { rolls; bids = b :: bids; called = false } ))
        in
        let call =
          if bids = [] then []
          else [ ("liar", Play { rolls; bids; called = true }) ]
        in
        Decision { player; infoset; actions = raises @ call }
end

let game config =
  (module Make (struct
    let config = config
  end) : Game.S)
