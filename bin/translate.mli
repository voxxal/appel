type level
type exp = unit
type access

val outermost: level
val new_level : level -> Temp.label -> bool list -> level
val formals: level -> access list
val alloc_local: level -> bool -> access