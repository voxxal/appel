module T = Tree

let linearize stm =
  let nop = T.EXP(CONST 0) in
  let commute a b = 
    match a, b with
      | T.EXP (T.CONST _), _ -> true
      | _, T.NAME _ -> true
      | _, T.CONST _ -> true
      | _ -> false
  in
  let ( % ) a b = 
    match a, b with
      | T.EXP (CONST _), b -> b
      | a, T.EXP (CONST _) -> a
      | a, b -> SEQ (a, b)
  in
  let rec reorder = function
    | [] -> (nop, [])
    | (T.CALL _ as call) :: rest -> 
      let t = Temp.new_temp () in
      reorder (ESEQ (MOVE (TEMP t, call), TEMP t) :: rest)
    | exp :: rest ->
      let stm, exp = do_exp exp in
      let rest_stm, rest_exp = reorder rest in
      if commute rest_stm exp then 
        (stm % rest_stm, exp :: rest_exp)
      else 
        let t = Temp.new_temp() in
        (stm % MOVE (TEMP t, exp) % rest_stm, TEMP t :: rest_exp)
  and reorder_stm subexps build =
    let stm, subexps' = reorder subexps in
    stm % build subexps'
  and reorder_exp subexps build =
    let stm, subexps' = reorder subexps in
    (stm, build subexps')
  and do_stm = function
    | T.SEQ (a, b) -> T.SEQ (do_stm a, do_stm b)
    | JUMP (e, labels) -> reorder_stm [e] (fun [e] -> JUMP (e, labels))
    | CJUMP (op, a, b, t, f) -> reorder_stm [a; b] (fun [a; b] -> CJUMP (op, a, b, t, f))
    | MOVE (TEMP t, CALL (f, args)) -> reorder_stm (f :: args) (fun (f :: args) -> MOVE (TEMP t, CALL (f, args)))
    | MOVE (TEMP t, b) -> reorder_stm [b] (fun [b] -> MOVE (TEMP t, b))
    | MOVE (MEM e, b) -> reorder_stm [e; b] (fun [e; b] -> MOVE (MEM e, b))
    | EXP (CALL (f, args)) -> reorder_stm (f :: args) (fun (f :: args) -> EXP (CALL (f, args)))
    | EXP e -> reorder_stm [e] (fun [e] -> T.EXP e)
    | s -> s
  and do_exp = function
    | T.BINOP (op, a, b) -> reorder_exp [a; b] (fun [a; b] -> T.BINOP (op, a, b))
    | MEM e -> reorder_exp [e] (fun [e] -> T.MEM e)
    | ESEQ (s, e) ->
      let sub_stms, e = do_exp e in
      (do_stm s % sub_stms, e)
    | CALL  (f, args) -> reorder_exp (f :: args) (fun (f :: args) -> T.CALL (f, args))
    | e -> (nop, e) 
  and linear l = function
    | T.SEQ (a, b) -> linear (linear l b) a
    | s -> s :: l
  in linear [] (do_stm stm)


type block = T.stm list

let basic_blocks stms: block list * Temp.label =
  let finish = Temp.new_label() in
  let rec blocks acc = function
    | (T.LABEL _ as head) :: tail ->
      let rec next curr_block = function
        | (T.JUMP _ as s) :: rest | (T.CJUMP _ as s) :: rest -> end_block rest (s :: curr_block)
        | T.LABEL lab :: _ as stms -> next curr_block (T.JUMP(T.NAME lab, [lab]) :: stms)
        | s :: rest -> next (s :: curr_block) rest
        | [] -> next curr_block [T.JUMP(T.NAME finish, [finish])]
      and end_block stms curr_block =
          blocks (List.rev curr_block :: acc) stms in
      next [head] tail
    | [] -> List.rev acc
    | stms -> blocks acc (T.LABEL (Temp.new_label ()) :: stms) in
  (blocks [] stms, finish)


let trace_schedule (blocks, finish) =
  let rec enter_block table = function
    | T.LABEL s :: _ as b -> Symbol.enter s b table
    | _ -> table
  and split_last = function
    | [x] -> ([], x)
    | h :: tail -> 
      let (tail', last) = split_last tail in
      (h :: tail', last)
  and trace table (T.LABEL label :: _ as block) rest =
    let table = Symbol.enter label [] table in
    match split_last block with
    | most, T.JUMP (T.NAME label, _) ->
      (match Symbol.look label table with
      | Some (_ :: _ as next_block) -> most @ trace table next_block rest
      | _ -> block @ get_next table rest)
    | most, T.CJUMP (oper, a, b, t, f) ->
      (match Symbol.look t table, Symbol.look f table with
      | _, Some (_ :: _ as b') -> block @ trace table b' rest
      | Some (_ :: _ as block'), _ -> 
        most @
        [T.CJUMP(T.not_rel oper, a, b, f, t)] @
        trace table block' rest
      | _ -> 
        let f' = Temp.new_label () in
        most @
        [T.CJUMP(oper, a, b, t, f'); T.LABEL f'; T.JUMP(T.NAME f, [f])] @
        get_next table rest)
    | _, T.JUMP _ -> block @ get_next table rest
    | _ -> ErrorMsg.impossible "invalid block passed to trace"
  and get_next table = function
    | (T.LABEL label :: _ as block) :: rest ->
      (match Symbol.look label table with
      | Some (_ :: _) -> trace table block rest
      | _ -> get_next table rest)
    | [] -> []
    in
  get_next (List.fold_left enter_block Symbol.empty (List.rev blocks)) blocks @ [T.LABEL finish]