module M = Map.Make (String)

type t = int M.t M.t

let empty = M.empty

let observe player moves t =
  List.fold_left
    (fun t (m : Play.move) ->
      if m.player <> player then t
      else
        M.update m.infoset
          (fun c ->
            let c = Option.value c ~default:M.empty in
            Some
              (M.update m.action
                 (fun n -> Some (1 + Option.value n ~default:0))
                 c))
          t)
    t moves

let count t key action =
  match M.find_opt key t with
  | None -> 0
  | Some c -> Option.value (M.find_opt action c) ~default:0

let visits t key =
  match M.find_opt key t with
  | None -> 0
  | Some c -> M.fold (fun _ n acc -> acc + n) c 0

let strategy ?(prior = 0.) (tree : Tree.t) player t =
  if prior < 0. then invalid_arg "Model.strategy: negative prior";
  Array.fold_left
    (fun s (i : Tree.infoset) ->
      if i.player <> player then s
      else
        let n = Array.length i.actions in
        let w =
          Array.map (fun a -> float_of_int (count t i.key a) +. prior) i.actions
        in
        let total = Array.fold_left ( +. ) 0. w in
        let probs =
          if total > 0. then Array.map (fun x -> x /. total) w
          else Array.make n (1. /. float_of_int n)
        in
        Strategy.add i.key ~actions:i.actions ~probs s)
    Strategy.empty tree.infosets
