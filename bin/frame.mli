type t
type access

type frag = PROC of { body: Tree.stm; frame: t } | STRING of Temp.label * string

val fp: Temp.t
val rv: Temp.t
val ra: Temp.t
val arg_regs: Temp.t list
val caller_saves: Temp.t list
val temp_map: string Temp.Table.t

val string: Symbol.t -> string -> string

val word_size: int
val exp: access -> Tree.exp -> Tree.exp

val new_frame: Temp.label -> bool list -> t
val formals: t -> access list
val alloc_local: t -> bool -> access

val external_call: string -> Tree.exp list -> Tree.exp

val proc_entry_exit_1: t -> Tree.stm -> Tree.stm
val proc_entry_exit_2: t -> Assem.instr list -> Assem.instr list