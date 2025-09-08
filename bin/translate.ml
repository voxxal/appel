type exp =
  | Ex of Tree.exp
  | Nx of Tree.stm
  | Cx of (Temp.label -> Temp.label -> Tree.stm)
(* prev frame / current frame *)
type level =  { parent: level; frame: Frame.t; id: unit ref }
type access = level * Frame.access

module T = Tree

let fragments = ref []

let rec outermost =
  { parent = outermost; frame = Frame.new_frame (Temp.named_label "$outermost") []; id = ref () }

let new_level (level: level) name formals: level = 
  { parent = level; frame = Frame.new_frame name (true :: formals); id = ref () }
let formals (level: level) = 
  let formals_no_sl = match Frame.formals level.frame with [] -> [] | sl :: rest -> rest in
  List.map (fun acc -> level, acc) formals_no_sl
let alloc_local (level: level) escape = level, Frame.alloc_local level.frame escape

let rec seq = function 
  | [] -> T.EXP (T.CONST 0)
  | [tr] -> tr
  | tr :: rest -> T.SEQ (tr, seq rest)

let un_ex = function
  | Ex e -> e
  | Nx s -> T.ESEQ (s, T.CONST 0)
  | Cx gen_stm ->
    let r = Temp.new_temp () in
    let t = Temp.new_label () in let f = Temp.new_label () in
    T.ESEQ (seq [
      T.MOVE (T.TEMP r, T.CONST 1);
      gen_stm t f;
      T.LABEL f;
      T.MOVE (T.TEMP r, T.CONST 0);
      T.LABEL t;
    ], T.TEMP r)

let un_nx = function
    | Ex e -> T.EXP e
    | Nx s -> s
    | Cx gen_stm -> 
      let label = Temp.new_label () in 
      seq [gen_stm label label; T.LABEL label]

let un_cx = function
    | Ex (T.CONST 0) -> (fun _ f -> T.JUMP (T.NAME f, [f]))
    | Ex (T.CONST 1) -> (fun t _ -> T.JUMP (T.NAME t, [t]))
    | Ex exp ->  (fun t f -> T.CJUMP (T.NE, exp, T.CONST 0, t, f))
    | Nx _ -> ErrorMsg.impossible "un_cx recieved nx"
    | Cx gen_stm -> gen_stm

let rec static_link def_level curr_level = 
  if def_level.id == curr_level.id then T.TEMP Frame.fp
  else Frame.exp (List.hd (Frame.formals curr_level.frame)) (static_link def_level curr_level.parent)

let simple_var (def_level, access) use_level = Ex (Frame.exp access (static_link def_level use_level))

let field_var base i = 
  Ex (T.MEM (T.BINOP (T.PLUS, un_ex base, T.CONST (i * Frame.word_size))))

let subscript_var arr sub =
  Ex (T.MEM (T.BINOP (T.PLUS, un_ex arr, T.BINOP (T.MUL, un_ex sub, T.CONST Frame.word_size))))

let nil_exp = Ex (T.CONST 0)

let int_exp i = Ex (T.CONST i)

let string_exp str = 
  let label = Temp.new_label () in
  fragments := Frame.STRING (label, str) :: !fragments;
  Ex (T.NAME label)

let arith_exp oper left right =
  let t_op = 
    match oper with
    | Absyn.PlusOp -> T.PLUS
    | MinusOp -> T.MINUS
    | TimesOp -> T.MUL
    | DivideOp -> T.DIV
    | _ -> ErrorMsg.impossible "unreachable"
  in
    Ex (T.BINOP (t_op, un_ex left, un_ex right))
let rel_exp oper left right =
  let t_op = 
    match oper with
    | Absyn.LtOp -> T.LT
    | LeOp -> T.LE
    | GtOp -> T.GT
    | GeOp -> T.GE
    | EqOp -> T.EQ
    | NeqOp -> T.NE
    | _ -> ErrorMsg.impossible "unreachable"
  in
    Cx (fun t f -> T.CJUMP (t_op, un_ex left, un_ex right, t, f))


let cond_exp test then' else' =
  let gen_stm = un_cx test in
  match then', else' with
  | then_t, None ->
    let then_stm = un_nx then_t in
    let t = Temp.new_label () in
    let f = Temp.new_label () in 
    Nx (seq [gen_stm t f; T.LABEL t; then_stm; T.LABEL f])
  | Nx then_stm, Some (Nx else_stm) ->
    let t = Temp.new_label () in
    let f = Temp.new_label () in 
    let join = Temp.new_label () in
    Nx (seq [
      gen_stm t f;
      T.LABEL f;
      else_stm;
      T.JUMP (T.NAME join, [join]);
      T.LABEL t;
      then_stm;
      T.LABEL join;
    ])
  | Ex then_exp, Some (Ex else_exp) ->
    let r = Temp.new_temp () in
    let t = Temp.new_label () in
    let f = Temp.new_label () in 
    let join = Temp.new_label () in
    Ex (T.ESEQ (seq [
      gen_stm t f;
      T.LABEL f;
      T.MOVE (T.TEMP r, else_exp);
      T.JUMP (T.NAME join, [join]);
      T.LABEL t;
      T.MOVE (T.TEMP r, then_exp);
      T.LABEL join;
    ], T.TEMP r))
  (* The example for this branch makes me so confused so i think this is what its saying *)
  (* TODO make the false label appear first after gen_stm *)
  | Cx then_cond, Some (Ex else_exp) ->
    let r = Temp.new_temp () in
    let f = Temp.new_label () in
    let t = Temp.new_label () in
    let join = Temp.new_label () in
    let zero = Temp.new_label () in
    Ex (T.ESEQ (seq [
      gen_stm t f;
      T.LABEL t;
      T.MOVE (T.TEMP r, T.CONST 1);
      then_cond join zero;
      T.LABEL zero;
      T.MOVE (T.TEMP r, T.CONST 0);
      T.JUMP (T.NAME join, [join]);
      T.LABEL f;
      T.MOVE (T.TEMP r, else_exp);
      T.LABEL join;
    ], T.TEMP r))
  | Ex then_exp, Some (Cx else_cond) ->
    let r = Temp.new_temp () in
    let f = Temp.new_label () in
    let t = Temp.new_label () in
    let join = Temp.new_label () in
    let zero = Temp.new_label () in
    Ex (T.ESEQ (seq [
      gen_stm t f;
      T.LABEL t;
      T.MOVE (T.TEMP r, then_exp);
      T.JUMP (T.NAME join, [join]);
      T.LABEL f;
      T.MOVE (T.TEMP r, T.CONST 1);
      else_cond join zero;
      T.LABEL zero;
      T.MOVE (T.TEMP r, T.CONST 0);
      T.LABEL join;
    ], T.TEMP r))
  | _ -> ErrorMsg.impossible "unreachable"


let record_exp fields =
  let r = Temp.new_temp () in
  let alloc = 
    T.MOVE (T.TEMP r, Frame.external_call "malloc" [T.CONST (List.length fields * Frame.word_size)]) in
  let build_field (acc, index) field = 
    (T.MOVE (T.MEM (T.BINOP (T.PLUS, T.TEMP r, T.CONST (index * Frame.word_size))), un_ex field) :: acc, index + 1)
  in
  let (constructs, _) = List.fold_left build_field ([], 0) fields in
  Ex (T.ESEQ (seq (alloc :: constructs), T.TEMP r))

let array_exp size init =
  let r = Temp.new_temp () in
  Ex (T.ESEQ (seq [T.MOVE (T.TEMP r, Frame.external_call "init_array" [un_ex size; un_ex init])], T.TEMP r))

let while_exp breakpoint test body =
  let gen_stm = un_cx test in
  let body_stm = un_nx body in
  let test_label = Temp.new_label () in
  let body_label = Temp.new_label () in
  Nx (seq [
    T.LABEL test_label;
    gen_stm body_label breakpoint;
    T.LABEL body_label;
    body_stm;
    T.JUMP (T.NAME test_label, [test_label]);
    T.LABEL breakpoint;
  ])

let break_exp breakpoint = 
  Nx (T.JUMP (T.NAME breakpoint, [breakpoint]))

let call_exp def_level use_level label args =
  let sl = static_link def_level use_level in
  Ex (T.CALL (T.NAME label, sl :: List.map un_ex args))

let seq_exp = function
  | [] -> Nx (T.EXP (T.CONST 0))
  | exps ->
    match List.rev exps with
    | last :: rest ->
      let rest = List.rev rest in
      Ex (T.ESEQ (seq (List.map un_nx rest), un_ex last))
    | [] -> ErrorMsg.impossible "wtf??"

let assign_exp var value =
  Nx (T.MOVE (un_ex var, un_ex value))

let let_exp exps body_exp = 
  Ex (T.ESEQ (seq (List.map un_nx exps), un_ex body_exp))


let proc_entry_exit {frame; _} body =
  let body' = Frame.proc_entry_exit_1 frame (T.MOVE (T.TEMP Frame.rv, un_ex body)) in
  fragments := Frame.PROC {frame; body= body'} :: !fragments

let get_result () = !fragments