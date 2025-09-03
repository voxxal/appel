type level
type exp
type access

val outermost: level
val new_level: level -> Temp.label -> bool list -> level
val formals: level -> access list
val alloc_local: level -> bool -> access


val simple_var: access -> level -> exp
val field_var: exp -> int -> exp
val subscript_var: exp -> exp -> exp

val nil_exp: exp
val int_exp: int -> exp
val string_exp: string -> exp

val arith_exp: Absyn.oper -> exp -> exp -> exp
val rel_exp: Absyn.oper -> exp -> exp -> exp

val cond_exp: exp -> exp -> exp option -> exp

val record_exp: exp list -> exp
val array_exp: exp -> exp -> exp

val while_exp: Temp.label -> exp -> exp -> exp
val break_exp: Temp.label -> exp

val call_exp: level -> level -> Temp.label -> exp list -> exp
val seq_exp: exp list -> exp
val assign_exp: exp -> exp -> exp

val let_exp: exp list -> exp -> exp

val proc_entry_exit: level -> exp -> unit
val get_result: unit -> Frame.frag list