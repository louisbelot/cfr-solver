type node =
  | Leaf of float array
  | Chance of float array * node array
  | Decision of int * node array

type t = {
  root : node;
  size : int;
  height : int;
  width : int;
  keys : string array;
  labels : string array array;
  owner : int array;
  offset : int array;
}

let build (module G : Multi.S) =
  let index = Hashtbl.create 256 and infos = ref [] in
  let size = ref 0 and height = ref 0 and width = ref 1 in
  let id key player actions =
    match Hashtbl.find_opt index key with
    | Some i -> i
    | None ->
        let i = Hashtbl.length index in
        Hashtbl.add index key i;
        infos := (key, player, Array.of_list (List.map fst actions)) :: !infos;
        i
  in
  let rec go h d =
    incr size;
    height := max !height d;
    let children l = Array.of_list (List.map (fun (_, h') -> go h' (d + 1)) l) in
    match G.node h with
    | Multi.Terminal u -> Leaf u
    | Chance outcomes ->
        Chance (Array.of_list (List.map fst outcomes), children outcomes)
    | Decision { player; infoset; actions } ->
        width := max !width (List.length actions);
        Decision (id infoset player actions, children actions)
  in
  let root = go G.root 0 in
  let infos = Array.of_list (List.rev !infos) in
  let labels = Array.map (fun (_, _, a) -> a) infos in
  let offset = Array.make (Array.length infos + 1) 0 in
  Array.iteri (fun i a -> offset.(i + 1) <- offset.(i) + Array.length a) labels;
  {
    root;
    size = !size;
    height = !height;
    width = !width;
    keys = Array.map (fun (k, _, _) -> k) infos;
    labels;
    owner = Array.map (fun (_, p, _) -> p) infos;
    offset;
  }

let slots t = t.offset.(Array.length t.keys)
