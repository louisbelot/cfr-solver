type config = {
  items : int;
  bribe : int;
  rounds : int;
  value : int;
  fine : int;
  compensation : int;
}

let baseline =
  { items = 10; bribe = 2; rounds = 2; value = 5; fine = 1; compensation = 1 }

let name c = Printf.sprintf "sheriff-%d-%d-%d" c.items c.bribe c.rounds

type history = { load : int; bribes : int list; inspects : bool list }

let public h =
  let rec go bs is =
    match (bs, is) with
    | b :: bs, i :: is ->
        Printf.sprintf "%d%c" b (if i then 'i' else 'p') :: go bs is
    | [ b ], [] -> [ string_of_int b ]
    | _ -> []
  in
  String.concat "," (go (List.rev h.bribes) (List.rev h.inspects))

let payoff c h =
  let n = h.load in
  match (h.bribes, h.inspects) with
  | b :: _, false :: _ -> [| float_of_int ((n * c.value) - b); float_of_int b |]
  | _, true :: _ when n > 0 ->
      [| float_of_int (-n * c.fine); float_of_int (n * c.fine) |]
  | _ -> [| float_of_int c.compensation; float_of_int (-c.compensation) |]

let game c =
  if c.items < 0 || c.bribe < 0 || c.rounds < 1 then
    invalid_arg "Sheriff.game: config";
  (module struct
    type nonrec history = history

    let name = name c
    let players = 2
    let root = { load = -1; bribes = []; inspects = [] }

    let node h =
      if h.load < 0 then
        Multi.Decision
          {
            player = 0;
            infoset = "load";
            actions =
              List.init (c.items + 1) (fun n ->
                  (string_of_int n, { h with load = n }));
          }
      else if List.length h.bribes = List.length h.inspects then
        if List.length h.inspects = c.rounds then Terminal (payoff c h)
        else
          Decision
            {
              player = 0;
              infoset = Printf.sprintf "%d:%s" h.load (public h);
              actions =
                List.init (c.bribe + 1) (fun b ->
                    (string_of_int b, { h with bribes = b :: h.bribes }));
            }
      else
        Decision
          {
            player = 1;
            infoset = "?" ^ public h;
            actions =
              [
                ("pass", { h with inspects = false :: h.inspects });
                ("inspect", { h with inspects = true :: h.inspects });
              ];
          }
  end : Multi.S)
