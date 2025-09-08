type t = int
let temp_count = ref 0
let new_temp () = temp_count := !temp_count + 1; !temp_count

let make_string temp = "t" ^ string_of_int temp

type label = Symbol.t
let label_count = ref 0
let new_label () = label_count := !label_count + 1; Symbol.symbol ("L" ^ string_of_int !label_count)
let named_label (str: string) = Symbol.symbol str
module Table = Map.Make (struct 
  type t = int

  let compare = Stdlib.compare
end)