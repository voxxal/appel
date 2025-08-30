type t = string * int

(* exception Symbol *)
let nextsym = ref 0
let sizeHint = 128
let hashtable = Hashtbl.create sizeHint

let symbol name = 
  match Hashtbl.find_opt hashtable name with
   | Some i -> (name, i)
   | None -> 
       let i = !nextsym in nextsym := i + 1;
       Hashtbl.add hashtable name i;
       (name, i)

let name(s,_) = s

let get_int ((_, i): t) = i;

module Table = Map.Make(struct 
  type t = string * int
  let compare s1 s2 = get_int(s1) - get_int(s2) 
end)

type 'a table = 'a Table.t
let empty = Table.empty
let enter = Table.add
let look = Table.find_opt

let iter = Table.iter
let of_list = Table.of_list