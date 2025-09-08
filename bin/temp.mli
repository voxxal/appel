type t = int
val new_temp: unit -> t
val make_string: t -> string

type label = Symbol.t
val new_label: unit -> label
val named_label: string -> label
module Table: Map.S with type key = int