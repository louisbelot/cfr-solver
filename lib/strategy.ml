module M = Map.Make (String)

type entry = { actions : string array; probs : float array }
type t = entry M.t

let empty = M.empty

let add key ~actions ~probs t =
  if Array.length actions <> Array.length probs then
    invalid_arg "Strategy.add: actions and probs differ in length";
  M.add key { actions; probs } t

let find t key = M.find_opt key t

let probs t key ~n =
  match M.find_opt key t with
  | None -> Array.make n (1. /. float_of_int n)
  | Some e when Array.length e.probs = n -> e.probs
  | Some _ -> invalid_arg ("Strategy.probs: wrong action count at " ^ key)

let union a b = M.union (fun _ x _ -> Some x) a b
let bindings = M.bindings
let cardinal = M.cardinal

let pp ?keys ppf t =
  let keys =
    match keys with Some k -> k | None -> List.map fst (bindings t)
  in
  let width = List.fold_left (fun w k -> max w (String.length k)) 0 keys in
  List.iter
    (fun key ->
      match find t key with
      | None -> Format.fprintf ppf "  %-*s  (uniform)@\n" width key
      | Some { actions; probs } ->
          Format.fprintf ppf "  %-*s" width key;
          Array.iteri
            (fun a p -> Format.fprintf ppf "  %s %.3f" actions.(a) p)
            probs;
          Format.fprintf ppf "@\n")
    keys
