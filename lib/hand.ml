type card = int

let ranks = "23456789TJQKA"
let suits = "cdhs"
let rank c = c lsr 2
let suit c = c land 3
let card r s = (r lsl 2) lor s

let of_string s =
  match
    ( String.length s,
      String.index_opt ranks (Char.uppercase_ascii s.[0]),
      String.index_opt suits (Char.lowercase_ascii s.[1]) )
  with
  | 2, Some r, Some s -> card r s
  | _ -> invalid_arg ("Hand.of_string: " ^ s)
  | exception Invalid_argument _ -> invalid_arg ("Hand.of_string: " ^ s)

let to_string c = Printf.sprintf "%c%c" ranks.[rank c] suits.[suit c]

let parse s =
  String.split_on_char ' ' s
  |> List.filter (( <> ) "")
  |> List.map of_string |> Array.of_list

type category =
  | High_card
  | Pair
  | Two_pair
  | Trips
  | Straight
  | Flush
  | Full_house
  | Quads
  | Straight_flush

let categories =
  [|
    High_card;
    Pair;
    Two_pair;
    Trips;
    Straight;
    Flush;
    Full_house;
    Quads;
    Straight_flush;
  |]

let category v = categories.(v lsr 20)

let category_name = function
  | High_card -> "high card"
  | Pair -> "pair"
  | Two_pair -> "two pair"
  | Trips -> "three of a kind"
  | Straight -> "straight"
  | Flush -> "flush"
  | Full_house -> "full house"
  | Quads -> "four of a kind"
  | Straight_flush -> "straight flush"

let rec popcount m = if m = 0 then 0 else 1 + popcount (m land (m - 1))

let rec top m r =
  if r < 0 then -1 else if m land (1 lsl r) <> 0 then r else top m (r - 1)

let straight_of m =
  let ext = (m lsl 1) lor ((m lsr 12) land 1) in
  let rec go r =
    if r < 3 then -1
    else if (ext lsr (r - 3)) land 0x1f = 0x1f then r
    else go (r - 1)
  in
  go 12

let bits = Array.init 8192 popcount
let high = Array.init 8192 (fun m -> top m 12)
let straights = Array.init 8192 straight_of
let[@inline] highest m = Array.unsafe_get high m
let[@inline] straight m = Array.unsafe_get straights m
let[@inline] count m = Array.unsafe_get bits m

let kickers n m =
  let rec go m n acc =
    if n = 0 then acc
    else if m = 0 then go m (n - 1) (acc lsl 4)
    else
      let r = highest m in
      go (m land lnot (1 lsl r)) (n - 1) ((acc lsl 4) lor r)
  in
  go m n 0

let make cat ranks = (cat lsl 20) lor ranks

let eval cards =
  let n = Array.length cards in
  if n < 5 || n > 7 then invalid_arg "Hand.eval: 5 to 7 cards";
  let s0 = ref 0 and s1 = ref 0 and s2 = ref 0 and s3 = ref 0 in
  let once = ref 0 and twice = ref 0 and thrice = ref 0 and four = ref 0 in
  for i = 0 to n - 1 do
    let c = cards.(i) in
    let b = 1 lsl rank c in
    (match suit c with
    | 0 -> s0 := !s0 lor b
    | 1 -> s1 := !s1 lor b
    | 2 -> s2 := !s2 lor b
    | _ -> s3 := !s3 lor b);
    four := !four lor (!thrice land b);
    thrice := !thrice lor (!twice land b);
    twice := !twice lor (!once land b);
    once := !once lor b
  done;
  let flush =
    if count !s0 >= 5 then !s0
    else if count !s1 >= 5 then !s1
    else if count !s2 >= 5 then !s2
    else if count !s3 >= 5 then !s3
    else 0
  in
  let all = !once and quads = !four in
  let trips = !thrice land lnot quads in
  let pairs = !twice land lnot !thrice in
  let sf = if flush = 0 then -1 else straight flush in
  if sf >= 0 then make 8 (sf lsl 16)
  else if quads <> 0 then
    let q = highest quads in
    make 7 ((q lsl 16) lor (kickers 1 (all land lnot (1 lsl q)) lsl 12))
  else if trips <> 0 && (count trips >= 2 || pairs <> 0) then
    let t = highest trips in
    let p = highest (trips land lnot (1 lsl t) lor pairs) in
    make 6 ((t lsl 16) lor (p lsl 12))
  else if flush <> 0 then make 5 (kickers 5 flush)
  else
    let st = straight all in
    if st >= 0 then make 4 (st lsl 16)
    else if trips <> 0 then
      let t = highest trips in
      make 3 ((t lsl 16) lor (kickers 2 (all land lnot (1 lsl t)) lsl 8))
    else if count pairs >= 2 then
      let p1 = highest pairs in
      let p2 = highest (pairs land lnot (1 lsl p1)) in
      let rest = all land lnot ((1 lsl p1) lor (1 lsl p2)) in
      make 2 ((p1 lsl 16) lor (p2 lsl 12) lor (kickers 1 rest lsl 8))
    else if pairs <> 0 then
      let p = highest pairs in
      make 1 ((p lsl 16) lor (kickers 3 (all land lnot (1 lsl p)) lsl 4))
    else make 0 (kickers 5 all)
