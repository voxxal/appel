module T = Types
type ty = T.t
type entry =
  | VarEntry of { ty: ty }
  | FunEntry of { formals: ty list; result: ty }

let base_tenv: ty Symbol.table = Symbol.of_list [(Symbol.symbol "int", T.INT); (Symbol.symbol "string", T.STRING)]
let base_venv: entry Symbol.table = 
  Symbol.of_list [
    (Symbol.symbol "print", FunEntry { formals = [T.STRING]; result = T.UNIT });
    (Symbol.symbol "flush", FunEntry { formals = []; result = T.UNIT });
    (Symbol.symbol "getchar", FunEntry { formals = []; result = T.STRING });
    (Symbol.symbol "ord", FunEntry { formals = [T.STRING]; result = T.INT });
    (Symbol.symbol "chr", FunEntry { formals = [T.INT]; result = T.STRING });
    (Symbol.symbol "size", FunEntry { formals = [T.STRING]; result = T.INT });
    (Symbol.symbol "substring", FunEntry { formals = [T.STRING; T.INT; T.INT]; result = T.STRING });
    (Symbol.symbol "concat", FunEntry { formals = [T.STRING; T.STRING]; result = T.STRING });
    (Symbol.symbol "not", FunEntry { formals = [T.INT]; result = T.INT });
    (Symbol.symbol "exit", FunEntry { formals = [T.INT]; result = T.UNIT });
  ]