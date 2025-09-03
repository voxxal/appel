type t
type access

type frag = PROC of { body: Tree.stm; frame: t } | STRING of Temp.label * string

val fp: Temp.t
val rv: Temp.t

val word_size: int
val exp: access -> Tree.exp -> Tree.exp

val new_frame: Temp.label -> bool list -> t
val formals: t -> access list
val alloc_local: t -> bool -> access

val external_call: string -> Tree.exp list -> Tree.exp

val proc_entry_exit_1: t -> Tree.stm -> Tree.stm