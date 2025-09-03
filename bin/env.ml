module T = Types
type ty = T.t
type entry =
  | VarEntry of { access: Translate.access; ty: ty }
  | FunEntry of { level: Translate.level; label: Temp.label; formals: ty list; result: ty }

let base_tenv: ty Symbol.table = Symbol.of_list [(Symbol.symbol "int", T.INT); (Symbol.symbol "string", T.STRING)]

let create_fun label formals result = 
  let temp_label = Temp.named_label label in
  Symbol.symbol label, 
  FunEntry {
    formals;
    result;
    label = temp_label;
    level = Translate.new_level Translate.outermost temp_label (List.map (fun f -> false) formals) 
  }
let base_venv: entry Symbol.table = 
  Symbol.of_list [
    create_fun "print" [T.STRING] T.UNIT;
    create_fun "flush" [] T.UNIT;
    create_fun "getchar" [] T.STRING;
    create_fun "ord" [T.STRING] T.INT;
    create_fun "chr" [T.INT] T.STRING;
    create_fun "size" [T.STRING] T.INT;
    create_fun "substring" [T.STRING; T.INT; T.INT] T.STRING;
    create_fun "concat" [T.STRING; T.STRING] T.STRING;
    create_fun "not" [T.INT] T.INT;
    create_fun "exit" [T.INT] T.UNIT;
  ]