type access = InFrame of int | InReg of Temp.t
type t = { formals: access list; locals: int ref; label: Temp.label } 

type frag = PROC of { body: Tree.stm; frame: t } | STRING of Temp.label * string

module T = Tree

let fp = Temp.new_temp ()
let rv = Temp.new_temp ()
(* rv64 *)
let word_size = 8
(* a0 - a7 *)
let max_args_in_regs = 8

let exp access exp = 
  match access with
  | InFrame k -> T.MEM(BINOP(PLUS, exp, CONST(k)))
  | InReg t -> T.TEMP t

let alloc_formals (acc, offset, in_regs) escape = 
  if escape || in_regs >= max_args_in_regs then
    (InFrame offset :: acc, offset + word_size, in_regs)
  else 
    (InReg (Temp.new_temp ()) :: acc, offset, in_regs + 1)

let new_frame label formals = 
  let (formals, _, _) = List.fold_left alloc_formals ([], 0, 0) formals in
  { formals; label; locals = ref 0 }
let formals frame = frame.formals

let alloc_local {locals;_} escape =
  if escape then 
    (locals := !locals + 1; InFrame (!locals * word_size))
  else
    InReg (Temp.new_temp ())

let external_call name args = T.CALL (T.NAME (Temp.named_label name), args)

let proc_entry_exit_1 frame body = body