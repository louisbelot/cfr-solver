open Cfr_solver

type history =
  | Root
  | At of {
      hands : (Hand.card * Hand.card) array;
      dealt : Hand.card list;
      path : string;
      node : Bet_tree.node;
    }

let conflict (a, b) (c, d) = a = c || a = d || b = c || b = d

let make ?(label = fun _ _ _ -> None) cfg ~board (r0 : Range.t) (r1 : Range.t) =
  let clean (r : Range.t) =
    let w = Array.make 1326 0. in
    Array.iteri (fun j c -> w.(Range.index c) <- r.weights.(j)) r.hands;
    Range.of_weights ~board w
  in
  let r0 = clean r0 and r1 = clean r1 in
  let tree = Bet_tree.build cfg in
  let pairs =
    List.concat
      (List.init (Range.size r0) (fun i ->
           List.filter_map
             (fun j ->
               if conflict r0.hands.(i) r1.hands.(j) then None
               else
                 Some
                   ( r0.weights.(i) *. r1.weights.(j),
                     [| r0.hands.(i); r1.hands.(j) |] ))
             (List.init (Range.size r1) Fun.id)))
  in
  let z = List.fold_left (fun acc (w, _) -> acc +. w) 0. pairs in
  let module G = struct
    type nonrec history = history

    let name = "subgame"
    let root = Root

    let showdown hands dealt win =
      let value (a, b) =
        Hand.eval (Array.of_list ((a :: b :: Array.to_list board) @ dealt))
      in
      compare (value hands.(0)) (value hands.(1)) |> float_of_int |> ( *. ) win

    let node = function
      | Root ->
          Game.Chance
            (List.map
               (fun (w, hands) ->
                 (w /. z, At { hands; dealt = []; path = ""; node = tree }))
               pairs)
      | At { hands; dealt; path; node } -> (
          match node with
          | Bet_tree.Folded { folder; win } ->
              Game.Terminal (if folder = 0 then -.win else win)
          | Showdown { win } -> Terminal (showdown hands dealt win)
          | Chance child ->
              let used c =
                Array.mem c board || List.mem c dealt
                || Array.exists (fun (a, b) -> a = c || b = c) hands
              in
              let cards =
                List.filter (fun c -> not (used c)) (List.init 52 Fun.id)
              in
              let p = 1. /. float_of_int (List.length cards) in
              Chance
                (List.map
                   (fun c ->
                     ( p,
                       At
                         {
                           hands;
                           dealt = dealt @ [ c ];
                           path = path ^ "/" ^ Hand.to_string c;
                           node = child;
                         } ))
                   cards)
          | Decision { player; actions; children; _ } ->
              Decision
                {
                  player = (if player = 0 then Game.P0 else P1);
                  infoset =
                    (match label player dealt hands.(player) with
                    | Some b -> Printf.sprintf "b%d:%s" b path
                    | None -> Subgame.key hands.(player) path);
                  actions =
                    Array.to_list
                      (Array.mapi
                         (fun a child ->
                           let label = Bet_tree.action_to_string actions.(a) in
                           ( label,
                             At
                               {
                                 hands;
                                 dealt;
                                 path = path ^ label;
                                 node = child;
                               } ))
                         children);
                })
  end in
  (module G : Game.S)
