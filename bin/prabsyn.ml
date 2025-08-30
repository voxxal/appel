module A = Absyn

let print outchan e0 =
  let say = output_string outchan in
  let sayf fmt = Printf.fprintf outchan fmt in
  let sayln s = (say s; say "\n") in

  let rec indent = function
    | 0 -> ()
    | i -> (say " "; indent (i - 1))
  in

  let opname = function
    | A.PlusOp -> "PlusOp"
    | A.MinusOp -> "MinusOp"
    | A.TimesOp -> "TimesOp"
    | A.DivideOp -> "DivideOp"
    | A.EqOp -> "EqOp"
    | A.NeqOp -> "NeqOp"
    | A.LtOp -> "LtOp"
    | A.LeOp -> "LeOp"
    | A.GtOp -> "GtOp"
    | A.GeOp -> "GeOp"
  in

  let rec dolist d f = function
    | [] -> ()
    | [a] -> (sayln ""; f a d)
    | (a :: r) -> (sayln ""; f a d; say ","; dolist d f r)
  in

  let rec var v d =
    indent d; 
    match v with
    | A.SimpleVar(s, _) -> sayf "SimpleVar(%s)" (Symbol.name s)
    | A.FieldVar(v, s, _) -> (
        sayln "FieldVar(";
        var v (d + 1);
        sayln ","; 
        indent(d + 1);
        sayf "%s)" (Symbol.name s)
      )
    | A.SubscriptVar(v, e, _) -> (
        sayln "SubscriptVar(";
        var v (d + 1); 
        sayln ","; 
        exp e (d + 1);
        say ")"
      )

  and exp e d =
    indent d; 
    match e with
    | A.VarExp v -> (
        sayln "VarExp(";
        var v (d + 1);
        say ")"
      )
    | A.NilExp -> say "NilExp"
    | A.IntExp i -> sayf "IntExp(%d)" i
    | A.StringExp (s, _) -> sayf "StringExp(\"%s\")" s
    | A.CallExp{func; args; _} -> (
        sayf "CallExp(%s,[" (Symbol.name func);
        dolist d exp args; 
        say "])"
      )
    | A.OpExp{left; oper; right; _} -> (
        sayf "OpExp(\n%s,\n" (opname oper); 
        exp left (d + 1); 
        sayln ","; 
        exp right (d + 1);
        say ")"
      )
    | A.RecordExp{fields; typ; _} -> (
        let f (name, e, _) d = 
          indent d; 
          sayf "(%s,\n" (Symbol.name name); 
          exp e (d + 1);
          say ")"
        in 
        sayf "RecordExp(%s,[\n" (Symbol.name typ); 
        dolist d f fields; 
        say "])"
      )
    | A.SeqExp l -> (
        say "SeqExp["; 
        dolist (d + 1) exp (List.map (fun (e, _) -> e) l); 
        say "]"
      )
    | A.AssignExp{var=v; exp=e; _} -> (
        sayln "AssignExp("; 
        var v (d + 1); 
        sayln ",";
        exp e (d + 1); 
        say ")"
      )
    | A.IfExp{test; then'; else'; _} -> (
        sayln "IfExp("; 
        exp test (d + 1); 
        sayln ",";
        exp then' (d + 1);
        (match else' with 
          | None -> ()
          | Some e -> (sayln ","; exp e (d + 1)));
        say ")"
      )
    | A.WhileExp{test; body; _} -> (
        sayln "WhileExp("; 
        exp test (d + 1); 
        sayln ",";
        exp body (d + 1); 
        say ")"
      )
    | A.ForExp{var; escape; lo; hi; body; _} -> (
        sayf "ForExp(\n%s,%B,\n" (Symbol.name var) !escape;
        exp lo (d + 1); 
        sayln ","; 
        exp hi (d + 1); 
        sayln ",";
        exp body (d + 1); 
        say ")"
      )
    | A.BreakExp _ -> say "BreakExp"
    | A.LetExp{decs; body; _} -> (
        say "LetExp([";
        dolist (d + 1) dec decs; 
        sayln "],"; 
        exp body (d + 1); 
        say")"
      )
    | A.ArrayExp{typ; size; init; _} -> (
        sayf "ArrayExp(%s,\n" (Symbol.name typ); 
        exp size (d + 1); 
        sayln ","; 
        exp init (d + 1); 
        say ")"
      )

  and dec d_node d = 
    indent d; 
    match d_node with
    | A.FunctionDec l -> (
        let field_f {A.name; escape; typ; _} d = 
          indent d; 
          sayf "(%s, %B, %s)" (Symbol.name name) !escape (Symbol.name typ); 
        in
        let fundec_f {A.name; params; result; body; _} d =
          indent d; 
          sayf "(%s, [" (Symbol.name name); 
          dolist d field_f params; 
          sayln "],";
          (match result with 
            | None -> say "None"
            | Some (s, _) -> (sayf "Some(%s)" (Symbol.name s)));
          sayln ",";
          exp body (d + 1); 
          say ")"
        in 
        say "FunctionDec["; 
        dolist d fundec_f l; 
        say "]"
      )
    | A.VarDec{name; escape; typ; init; _} -> (
        sayf "VarDec(%s, %b, " (Symbol.name name) !escape; 
        (match typ with 
          | None -> say "None" 
          | Some (s, _) -> (sayf "Some(%s)" (Symbol.name s)));
        sayln ","; 
        exp init (d + 1); 
        say ")"
      )
    | A.TypeDec l -> (
        let typedec_f {A.name; ty=t; _} d = 
          indent d; 
          sayf "(%s, " (Symbol.name name); 
          ty t d; 
          say ")"
        in 
        say "TypeDec["; 
        dolist d typedec_f l; 
        say "]"
      )
  
  and ty t_node d =
    indent d; 
    match t_node with
    | A.NameTy(s, _) -> (
        sayf "NameTy(%s)" (Symbol.name s);
      )
    | A.RecordTy l -> (
        let field_f {A.name; escape; typ; _} d =
          indent d; 
          sayf "(%s, %B, %s)" (Symbol.name name) !escape (Symbol.name typ); 
        in 
        say "RecordTy["; 
        dolist d field_f l; 
        say "]"
      )
    | A.ArrayTy(s, _) -> (
        sayf "ArrayTy(%s)" (Symbol.name s);
      )

  in  
  exp e0 0; 
  sayln ""; 
  flush outchan