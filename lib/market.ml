type config = { cards : int; hidden : int; width : int; rounds : int }
type side = Buy | Sell | Pass

let check c =
  if c.hidden < 0 || c.cards < 2 + c.hidden then
    invalid_arg "Market: need at least 2 + hidden cards";
  if c.width < 0 then invalid_arg "Market: width must be >= 0";
  if c.rounds < 1 || c.rounds > 2 then
    invalid_arg "Market: rounds must be 1 or 2"

let name c =
  Printf.sprintf "market-%d%s-w%d%s" c.cards
    (if c.hidden > 0 then Printf.sprintf "h%d" c.hidden else "")
    c.width
    (if c.rounds = 2 then "-2r" else "")

let dealt c = 2 + c.hidden
let sum_range lo k = k * ((2 * lo) + k - 1) / 2

let mids c =
  let n = dealt c in
  (sum_range 1 n, sum_range (c.cards - n + 1) n)

let fair c own =
  let deck = float_of_int (c.cards * (c.cards + 1) / 2) in
  float_of_int own
  +. float_of_int (dealt c - 1)
     *. (deck -. float_of_int own)
     /. float_of_int (c.cards - 1)

let half c = float_of_int c.width /. 2.

let pnl c side mid total =
  let m = float_of_int mid and s = float_of_int total in
  match side with
  | Buy -> s -. (m +. half c)
  | Sell -> m -. half c -. s
  | Pass -> 0.

let side_char = function Buy -> 'b' | Sell -> 's' | Pass -> 'p'

type history =
  | Deal
  | Play of {
      hands : int * int;
      total : int;
      rounds : (int * side) list;
      quote : int option;
    }

let rec draws k pool =
  if k = 0 then [ [] ]
  else
    List.concat_map
      (fun x ->
        List.map (List.cons x) (draws (k - 1) (List.filter (( <> ) x) pool)))
      pool

let public rounds quote =
  let done_ =
    List.rev_map (fun (m, s) -> Printf.sprintf "%d%c" m (side_char s)) rounds
  in
  String.concat ","
    (done_ @ match quote with Some m -> [ string_of_int m ] | None -> [])

module Make (C : sig
  val config : config
end) =
struct
  let c = C.config
  let () = check c

  type nonrec history = history

  let name = name c
  let root = Deal
  let lo, hi = mids c

  let deals =
    let all = draws (dealt c) (List.init c.cards (fun i -> i + 1)) in
    let p = 1. /. float_of_int (List.length all) in
    List.map
      (function
        | a :: b :: rest ->
            ( p,
              Play
                {
                  hands = (a, b);
                  total = List.fold_left ( + ) (a + b) rest;
                  rounds = [];
                  quote = None;
                } )
        | _ -> assert false)
      all

  let quoter r = if r mod 2 = 0 then Game.P0 else P1

  let payoff total rounds =
    List.fold_left
      (fun (acc, r) (m, s) ->
        let taker = pnl c s m total in
        let p0 = match quoter r with P0 -> -.taker | P1 -> taker in
        (acc +. p0, r - 1))
      (0., List.length rounds - 1)
      rounds
    |> fst

  let node = function
    | Deal -> Game.Chance deals
    | Play { total; rounds; quote = None; _ } when List.length rounds = c.rounds
      ->
        Game.Terminal (payoff total rounds)
    | Play ({ hands; rounds; quote; _ } as p) ->
        let r = List.length rounds in
        let player =
          match quote with None -> quoter r | Some _ -> Game.other (quoter r)
        in
        let own = match player with P0 -> fst hands | P1 -> snd hands in
        let infoset = Printf.sprintf "%d:%s" own (public rounds quote) in
        let actions =
          match quote with
          | None ->
              List.init
                (hi - lo + 1)
                (fun i ->
                  let m = lo + i in
                  (string_of_int m, Play { p with quote = Some m }))
          | Some m ->
              List.map
                (fun s ->
                  ( (match s with
                    | Buy -> "buy"
                    | Sell -> "sell"
                    | Pass -> "pass"),
                    Play { p with rounds = (m, s) :: rounds; quote = None } ))
                [ Buy; Sell; Pass ]
        in
        Decision { player; infoset; actions }
end

let game config =
  (module Make (struct
    let config = config
  end) : Game.S)

let quotes c f =
  let lo, hi = mids c in
  let actions = Array.init (hi - lo + 1) (fun i -> string_of_int (lo + i)) in
  List.fold_left
    (fun s own ->
      let m = Int.min hi (Int.max lo (f own)) in
      let probs =
        Array.init (hi - lo + 1) (fun k -> if lo + k = m then 1. else 0.)
      in
      Strategy.add (Printf.sprintf "%d:" own) ~actions ~probs s)
    Strategy.empty
    (List.init c.cards (fun i -> i + 1))

let nearest x = Float.to_int (Float.floor (x +. 0.5))
let revealing c = quotes c (fun own -> nearest (fair c own))

let uninformed c =
  let lo, hi = mids c in
  quotes c (fun _ -> nearest (float_of_int (lo + hi) /. 2.))
