type t = int
let temp_count = ref 0
let new_temp () = temp_count := !temp_count + 1; !temp_count

type label = string
let label_count = ref 0
let new_label () = label_count := !label_count + 1; "L" ^ string_of_int !label_count
let named_label (str: string) = str