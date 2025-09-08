type access = InFrame of int | InReg of Temp.t
type t = { formals: access list; locals: int ref; label: Temp.label } 

type frag = PROC of { body: Tree.stm; frame: t } | STRING of Temp.label * string

module T = Tree
type register = string

let a0, a1, a2, a3, a4, a5, a6, a7 = Temp.new_temp (), Temp.new_temp (), Temp.new_temp (), Temp.new_temp (), Temp.new_temp (), Temp.new_temp (), Temp.new_temp (), Temp.new_temp ()
let fp, rv, sp, ra = Temp.new_temp (), Temp.new_temp (), Temp.new_temp (), Temp.new_temp ()
let s2, s3, s4, s5, s6, s7, s8, s9, s10, s11 = Temp.new_temp (), Temp.new_temp (), Temp.new_temp (), Temp.new_temp (), Temp.new_temp (), Temp.new_temp (), Temp.new_temp (), Temp.new_temp (), Temp.new_temp (), Temp.new_temp ()
let t0, t1, t2, t3, t4, t5, t6 = Temp.new_temp (), Temp.new_temp (), Temp.new_temp (), Temp.new_temp (), Temp.new_temp (), Temp.new_temp (), Temp.new_temp ()
let zero = Temp.new_temp ()

let special_regs = [fp; sp; ra; rv]
let arg_regs = [a0; a1; a2; a3; a4; a5; a6; a7]
let callee_saves = [s2; s3; s4; s5; s6; s7; s8; s9; s10; s11]
let caller_saves = [t0; t1; t2; t3; t4; t5; t6]
(* NOTE rv and a0 share a register so if anything breaks that's why *)
let temp_map = Temp.Table.of_list [
  zero, "zero";
  a0, "a0"; a1, "a1"; a2, "a2"; a3, "a3"; a4, "a4"; a5, "a5"; a6, "a6"; a7, "a7";
  ra, "ra"; rv, "a0"; sp, "sp"; fp, "fp";
  s2, "s2"; s3, "s3"; s4, "s4"; s5, "s5"; s6, "s6"; s7, "s7"; s8, "s8"; s9, "s9"; s10, "s10"; s11, "s11";
  t0, "t0"; t1, "t1"; t2, "t2"; t3, "t3"; t4, "t4"; t5, "t5"; t6, "t6"
]

(* rv64 *)
let word_size = 8
(* a0 - a7 *)
let max_args_in_regs = 8

let string label str : string = Symbol.name label ^ ":\t.asciiz \"" ^ str ^ "\"\n"

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
let proc_entry_exit_2 frame instrs = 
  instrs @ [Assem.OPER { assem = ""; src = [zero; ra; sp] @ callee_saves; dst = []; jump = None }]
let proc_entry_exit_3 frame body = ()