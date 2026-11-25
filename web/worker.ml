open Js_of_ocaml
open Cfr_solver

let str m k = Js.to_string (Js.Unsafe.get m k)
let num m k = Js.float_of_number (Js.Unsafe.get m k)

let request m : Explorer.request =
  let id = int_of_float (num m "id") in
  match str m "kind" with
  | "create" ->
      Create
        {
          id;
          game = str m "game";
          algorithm = str m "algorithm";
          sampling = str m "sampling";
          seed = int_of_float (num m "seed");
          last = Js.to_bool (Js.Unsafe.get m "last");
        }
  | "step" ->
      Step
        { id; budget = num m "budget"; target = int_of_float (num m "target") }
  | "table" -> Table { id }
  | "history" -> History { id; key = str m "key" }
  | "deal" ->
      Deal
        {
          id;
          seat = int_of_float (num m "seat");
          seed = int_of_float (num m "seed");
        }
  | "act" -> Act { id; action = str m "action" }
  | "drop" -> Drop { id }
  | k -> invalid_arg ("unknown request " ^ k)

let now () = Js.float_of_number (Js.Unsafe.meth_call Js.date "now" [||])

let () =
  let e = Explorer.create ~now in
  Worker.set_onmessage (fun m ->
      let reply = Explorer.handle e (request m) in
      Worker.post_message (Js.string (Json.to_string reply)))
