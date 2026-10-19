type t =
  | Null
  | Bool of bool
  | Int of int
  | Float of float
  | String of string
  | List of t list
  | Object of (string * t) list

(* Shortest of %.15g, %.16g, %.17g that reads back to the same float, so 0.1
   prints as 0.1 and nothing is lost. *)
let float_to_string x =
  if not (Float.is_finite x) then invalid_arg "Json: non-finite float";
  let rec go p =
    let s = Printf.sprintf "%.*g" p x in
    if p >= 17 || float_of_string s = x then s else go (p + 1)
  in
  go 15

let escape buf s =
  Buffer.add_char buf '"';
  String.iter
    (function
      | '"' -> Buffer.add_string buf "\\\""
      | '\\' -> Buffer.add_string buf "\\\\"
      | '\n' -> Buffer.add_string buf "\\n"
      | c when Char.code c < 0x20 ->
          Buffer.add_string buf (Printf.sprintf "\\u%04x" (Char.code c))
      | c -> Buffer.add_char buf c)
    s;
  Buffer.add_char buf '"'

let is_scalar = function List _ | Object _ -> false | _ -> true

let rec write buf indent = function
  | Null -> Buffer.add_string buf "null"
  | Bool b -> Buffer.add_string buf (string_of_bool b)
  | Int i -> Buffer.add_string buf (string_of_int i)
  | Float x -> Buffer.add_string buf (float_to_string x)
  | String s -> escape buf s
  | List l -> block buf indent '[' ']' (List.map (fun x -> (None, x)) l)
  | Object kvs ->
      block buf indent '{' '}' (List.map (fun (k, x) -> (Some k, x)) kvs)

(* A container of scalars stays on one line; anything else gets one element
   per line. In a strategy file that puts each infoset on its own line. *)
and block buf indent opening closing items =
  let item (key, x) =
    Option.iter
      (fun k ->
        escape buf k;
        Buffer.add_string buf ": ")
      key;
    write buf (indent + 2) x
  in
  Buffer.add_char buf opening;
  (if List.for_all (fun (_, x) -> is_scalar x) items then
     List.iteri
       (fun i it ->
         if i > 0 then Buffer.add_string buf ", ";
         item it)
       items
   else
     let pad = String.make (indent + 2) ' ' in
     List.iteri
       (fun i it ->
         Buffer.add_string buf (if i > 0 then ",\n" else "\n");
         Buffer.add_string buf pad;
         item it)
       items;
     Buffer.add_char buf '\n';
     Buffer.add_string buf (String.make indent ' '));
  Buffer.add_char buf closing

let to_string v =
  let buf = Buffer.create 256 in
  write buf 0 v;
  Buffer.contents buf
