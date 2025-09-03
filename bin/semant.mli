type venv = Env.entry Symbol.table
type tenv = Types.t Symbol.table

type expty = Translate.exp * Types.t

val transProg: Absyn.exp -> Frame.frag list

(* val transVar: venv * tenv -> Absyn.var -> expty *)
val transExp : ?breakpoint:Temp.label option -> venv -> tenv -> Translate.level -> Absyn.exp -> expty
val transDecs: ?breakpoint:Temp.label option -> venv -> tenv -> Translate.level -> Absyn.dec list -> venv * tenv * Translate.exp list
val transTy  : tenv -> Absyn.ty  -> Types.t