module A = Assem
module T = Tree

let emit (a: A.instr) = ()

let fmt s = Printf.sprintf s
let impossible = ErrorMsg.impossible
let codegen _frame stm =
  let ilist = ref [] in
  let call_defs = [Frame.rv; Frame.ra] @ Frame.caller_saves in
  let emit x = ilist := x :: !ilist in
  let result gen =
      let t = Temp.new_temp () in
      gen t; t
  in
  let rec munch_stm = function
    | T.SEQ (a, b) -> munch_stm a; munch_stm b; ()
    | LABEL label -> emit (A.LABEL { assem = Symbol.name label ^ ":\n"; label })
    | JUMP (NAME label, _) ->
      emit (A.OPER {
        assem = "j `j0\n"; src = []; dst = []; jump = Some [label]
      })
    | JUMP (e1, labels) ->
      emit (A.OPER {
        assem = "j `s0\n"; src = [munch_exp e1]; dst = []; jump = Some labels
      })
    | CJUMP (op, e1, e2, t, f) ->
      let instr = match op with
        | EQ -> "beq"
        | NE -> "bne"
        | LT -> "blt"
        | GT -> "bgt"
        | LE -> "ble"
        | GE -> "bge"
        | ULT -> "bltu"
        | ULE -> "bleu"
        | UGT -> "bgtu"
        | UGE -> "bgeu"
      in
      emit (A.OPER {
        assem = fmt "%s `s0, `s1, `j0\n" instr;
        src = [munch_exp e1; munch_exp e2]; dst = []; jump = Some [t; f] 
      })
    | MOVE (MEM (BINOP (PLUS, CONST i, e1)), e2)
    | MOVE (MEM (BINOP (PLUS, e1, CONST i)), e2) -> 
      emit (A.OPER { 
        assem =  fmt "sd `s1, %d(`s0)\n" i;
        src = [munch_exp e1; munch_exp e2]; dst = []; jump = None;
      })
    | MOVE (MEM e1, MEM e2) ->
      emit (A.OPER {
        assem = "sd `s1, 0(`s0)\n";
        src = [munch_exp e1; munch_exp e2]; dst = []; jump = None;
      })
    | MOVE (MEM (CONST i), e2) ->
      emit (A.OPER {
        assem = fmt "sd `s0, %d(zero)\n" i;
        src = [munch_exp e2]; dst = []; jump = None;
      })
    | MOVE (MEM e1, e2) ->    
      emit (A.OPER {
        assem = "sd `s1, 0(`s0)\n";
        src = [munch_exp e1; munch_exp e2]; dst = []; jump = None;
      })
    | MOVE (TEMP t, CONST i) -> 
      emit (A.OPER {
        assem = fmt "li `d0, %d\n" i;
        src = []; dst = [t]; jump = None
      })

    | MOVE (TEMP t, e2) ->
      emit (A.MOVE {
        assem = "mv `d0, `s0\n";
        src = munch_exp e2; dst = t;
      })
    | MOVE _ -> impossible "invalid MOVE ir"
    | EXP e ->
      ignore (munch_exp e)
  and munch_exp = function
    | T.BINOP (PLUS, e1, CONST i) 
    | BINOP (PLUS, CONST i, e1) ->
      result (fun r -> emit (A.OPER {
        assem = fmt "addi `d0, `s0, %d\n" i;
        src = [munch_exp e1]; dst = [r]; jump = None
      }))
    | BINOP (PLUS, e1, e2) ->
      result (fun r ->  emit (A.OPER {
        assem = "add, `d0, `s0, `s1\n";
        src = [munch_exp e1; munch_exp e2]; dst = [r]; jump = None
      }))
    | BINOP (MINUS, e1, CONST i) ->
      result (fun r -> emit (A.OPER {
        assem = fmt "addi `d0, `s0, %d\n" (-i);
        src = [munch_exp e1]; dst = [r]; jump = None
      }))
    | BINOP (MINUS, e1, e2) -> 
      result (fun r ->  emit (A.OPER {
        assem = "sub, `d0, `s0, `s1\n";
        src = [munch_exp e1; munch_exp e2]; dst = [r]; jump = None
      }))
    | BINOP (MUL, e1, e2) ->
      result (fun r ->  emit (A.OPER {
        assem = "mul, `d0, `s0, `s1\n";
        src = [munch_exp e1; munch_exp e2]; dst = [r]; jump = None
      }))
    | BINOP (DIV, e1, e2) ->
      result (fun r ->  emit (A.OPER {
        assem = "div, `d0, `s0, `s1\n";
        src = [munch_exp e1; munch_exp e2]; dst = [r]; jump = None
      }))
    | BINOP _ -> impossible "binop pattern not implemented"
    | MEM (BINOP (PLUS, e1, CONST i))
    | MEM (BINOP (PLUS, CONST i, e1)) ->
      result (fun r -> emit (A.OPER {
        assem = fmt "ld `d0, %d(`s0)\n" i;
        src = [munch_exp e1]; dst = [r]; jump = None
      }))
    | MEM (CONST i) ->
      result (fun r -> emit (A.OPER {
        assem = fmt "ld `d0, %d(zero)\n" i;
        src = []; dst = [r]; jump = None
      }))
    | MEM e1 ->
      result (fun r -> emit (A.OPER {
        assem = "ld `d0, 0(`s0)\n";
        src = [munch_exp e1]; dst = [r]; jump = None
      }))
    | TEMP t -> t
    | ESEQ (s1, e1) -> impossible "ESEQ should have been canonicalized out"
    | NAME label ->
      result (fun r -> emit (A.OPER {
        assem = fmt "la `d0, %s\n" (Symbol.name label);
        src = []; dst = [r]; jump = None
      }))
    | CONST i ->
      result (fun r -> emit (A.OPER {
        assem = fmt "li `d0, %d\n" i;
        src = []; dst = [r]; jump = None
      }))
    | CALL (NAME label, args) -> 
      emit (A.OPER {
        assem = fmt "call %s\n" (Symbol.name label);
        src = munch_args 0 args; dst = call_defs; jump = None;
      });
      Frame.rv
  and munch_args i = function
    | [] -> []
    | arg :: rest ->
      if i < 8 then
        let dst = List.nth Frame.arg_regs i in
        munch_stm (MOVE (TEMP dst, arg));
        dst :: munch_args (i + 1) rest
      else impossible "frame args not impled" 
  in munch_stm stm;
  List.rev !ilist
