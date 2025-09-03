type venv = Env.entry Symbol.table
type tenv = Types.t Symbol.table

type expty = Translate.exp * Types.t

val transProg: Absyn.exp -> unit

(* val transVar: venv * tenv -> Absyn.var -> expty *)
val transExp: ?breakable:bool -> venv -> tenv -> Translate.level -> Absyn.exp -> expty
val transDec: venv -> tenv -> Translate.level -> Absyn.dec -> venv * tenv
val transTy : tenv -> Absyn.ty  -> Types.t