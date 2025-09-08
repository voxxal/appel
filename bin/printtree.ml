module T = Tree

let print_tree outstream s0 =
  let say s = output_string outstream s in
  let sayln s = say s; say "\n" in

  let rec indent i =
    if i > 0 then (say " "; indent (i - 1))
  in

  let rec stm s d = match s with
    | T.SEQ (a, b) ->
        indent d; sayln "SEQ(";
        stm a (d + 1); sayln ",";
        stm b (d + 1); say ")"
    | T.LABEL lab -> (indent d; say "LABEL "; say (Symbol.name lab))
    | T.JUMP (e, _) -> indent d; sayln "JUMP("; exp e (d + 1); say ")"
    | T.CJUMP (r, a, b, t, f) ->
        indent d; say "CJUMP(";
        relop r; sayln ",";
        exp a (d + 1); sayln ",";
        exp b (d + 1); sayln ",";
        indent (d + 1); say (Symbol.name t);
        say ","; say (Symbol.name f); say ")"
    | T.MOVE (a, b) ->
        indent d; sayln "MOVE(";
        exp a (d + 1); sayln ",";
        exp b (d + 1); say ")"
    | T.EXP e -> indent d; sayln "EXP("; exp e (d + 1); say ")"

  and exp e d = match e with
    | T.BINOP (p, a, b) ->
        indent d; say "BINOP(";
        binop p; sayln ",";
        exp a (d + 1); sayln ",";
        exp b (d + 1); say ")"
    | T.MEM e -> indent d; sayln "MEM("; exp e (d + 1); say ")"
    | T.TEMP t -> indent d; say "TEMP t"; say (string_of_int t)
    | T.ESEQ (s, e) ->
        indent d; sayln "ESEQ(";
        stm s (d + 1); sayln ",";
        exp e (d + 1); say ")"
    | T.NAME lab -> indent d; say "NAME "; say (Symbol.name lab)
    | T.CONST i -> indent d; say "CONST "; say (string_of_int i)
    | T.CALL (e, el) ->
        indent d; sayln "CALL(";
        exp e (d + 1);
        List.iter (fun a -> sayln ","; exp a (d + 2)) el;
        say ")"

  and binop = function
    | T.PLUS -> say "PLUS"
    | T.MINUS -> say "MINUS"
    | T.MUL -> say "MUL"
    | T.DIV -> say "DIV"
    | T.AND -> say "AND"
    | T.OR -> say "OR"
    | T.LSHIFT -> say "LSHIFT"
    | T.RSHIFT -> say "RSHIFT"
    | T.ARSHIFT -> say "ARSHIFT"
    | T.XOR -> say "XOR"

  and relop = function
    | T.EQ -> say "EQ"
    | T.NE -> say "NE"
    | T.LT -> say "LT"
    | T.GT -> say "GT"
    | T.LE -> say "LE"
    | T.GE -> say "GE"
    | T.ULT -> say "ULT"
    | T.ULE -> say "ULE"
    | T.UGT -> say "UGT"
    | T.UGE -> say "UGE"
  in
  stm s0 0;
  sayln "";
  flush outstream