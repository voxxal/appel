type venv = Env.entry Symbol.table
type tenv = Types.t Symbol.table

module A = Absyn
module E = Env
module Tr = Translate


type expty = Tr.exp * Types.t

let error = ErrorMsg.error
let impossible = ErrorMsg.impossible
let all_type (ty: Types.t) = List.for_all Types.((=) ty)

let print_tenv (tenv: tenv) = Symbol.iter (fun k v -> print_endline (Symbol.name k ^ " <type>:\n" ^ Types.format v ^ "\n")) tenv
let print_venv (venv: venv) = 
  Symbol.iter 
  (fun k v -> 
    print_endline (Symbol.name k ^ ":"); 
    match v with 
    | E.FunEntry {formals; result; _} -> 
      print_endline "Formals:";
      print_endline (String.concat ",\n" (List.map Types.format formals));
      print_endline "Result:";
      print_endline (Types.format result);
    | VarEntry {ty; _} -> print_endline (Types.format ty)) venv

let rec transProg exp =
  let main_level = Tr.new_level Tr.outermost (Symbol.symbol "main") [] in
  let exp, _ = transExp E.base_venv E.base_tenv main_level exp in
  Tr.proc_entry_exit main_level exp;
  Tr.get_result ()
and transDecs ?(breakpoint=None) venv tenv level decs =
  let transDec (venv, tenv, exps) = function
    | A.FunctionDec decs ->
      let param_type ({name; escape; typ; pos}: A.field) = 
        (match Symbol.look typ tenv with Some t -> (name, t)  | None -> error pos "type not found") in
      let res_type = function
        | Some (sym, pos) -> (match Symbol.look sym tenv with Some ty -> ty | None -> error pos "return type of function not found")
        | None -> Types.UNIT
      in
      let enter_header venv ({name; params; result; _} : A.fundec) =
        let label = Temp.new_label () in
        Symbol.enter name (E.FunEntry {
          level = Tr.new_level level label (List.map (fun (f: A.field) -> !(f.escape)) params);
          label;
          formals = List.map (fun p -> let (_, t) = param_type p in t) params;
          result = res_type result
        }) venv
      in
      let enter_param venv ((name, ty), access) = Symbol.enter name (E.VarEntry {access; ty}) venv in
      let process_fun venv ({name; params; result; body; pos;} : A.fundec) =
        let level' = match Symbol.look name venv with Some(E.FunEntry {level; _}) -> level | _ -> impossible "process_fun @ Semant.transDecs.transDec" in
        let params' = List.map param_type params in
        let body_venv = List.fold_left enter_param venv (List.combine params' (Tr.formals level')) in
        let body_exp, body_res_type = transExp ~breakpoint body_venv tenv level' body in
        if not Types.(body_res_type = res_type result) then error pos "result type did not match expected result type";
        Tr.proc_entry_exit level' body_exp;
        venv
      in
      let rec check_name_dupe found_names (decs : A.fundec list) =
        match decs with
        | [] -> ()
        | {name; pos; _} :: rest -> 
          if List.mem name found_names then
            error pos "duplicate name in function declarations"
          else check_name_dupe (name :: found_names) rest
      in
      check_name_dupe [] decs;
      let venv_headers = List.fold_left enter_header venv decs in
      let venv_final = List.fold_left process_fun venv_headers decs in
      (venv_final, tenv, exps)
    | VarDec {name; escape; typ = Some (type_sym, _); init; pos} -> 
      let access = Tr.alloc_local level !escape in
      let var_exp = Tr.simple_var access level in
      let init_exp, init_ty = transExp ~breakpoint venv tenv level init in
      let venv' = Symbol.enter name (E.VarEntry {access; ty = init_ty}) venv in
      (match Symbol.look type_sym tenv with
      | Some typ ->
        if not Types.(init_ty = typ) then error pos "type does not match expected type in var declaration";
        (venv', tenv, Tr.assign_exp var_exp init_exp :: exps)
      | None -> error pos "type in variable declaration is undefined")
    | VarDec {name; escape; typ = None; init; pos} -> 
      let access = Tr.alloc_local level !escape in
      let var_exp =  Tr.simple_var access level in
      let init_exp, init_ty = transExp ~breakpoint venv tenv level init in
      if init_ty = Types.NIL then error pos "variable initalized to nil with no type defined";
      (Symbol.enter name (E.VarEntry {access; ty = init_ty}) venv, tenv, Tr.assign_exp var_exp init_exp :: exps)
    | TypeDec decs -> 
      let enter_header tenv ({name; _} : A.typedec) = 
        Symbol.enter name (Types.NAME (name, ref None)) tenv
      in
      let enter_type tenv ({name; ty; _} : A.typedec) =
        match Symbol.look name tenv with
        | Some (Types.NAME (_, ty_hole)) -> ty_hole := Some (transTy tenv ty); tenv
        | Some _ | None -> Symbol.enter name (transTy tenv ty) tenv
      in
      let rec check_cycle tenv (decs : A.typedec list) =
        let rec traverse found_types current =
          match current with
          | Types.NAME (_, stored_ty) ->
            if List.memq stored_ty found_types then true
            else
              (match !stored_ty with
              | Some real_ty -> traverse (stored_ty :: found_types) real_ty
              | None -> false)
          | _ -> false
        in
        match decs with
        | [] -> ()
        | {ty; pos; _} :: rest ->
          if traverse [] (transTy tenv ty)  then
            error pos "cycle detected in type definitions"
          else check_cycle tenv rest
      in
      let rec check_name_dupe found_names (decs : A.typedec list) =
        match decs with
        | [] -> ()
        | {name; pos; _} :: rest -> 
          if List.mem name found_names then
            error pos "duplicate name in type definitions"
          else check_name_dupe (name :: found_names) rest
      in
      check_name_dupe [] decs;
      let tenv_headers = List.fold_left enter_header tenv decs in
      let tenv_final = List.fold_left enter_type tenv_headers decs in
      check_cycle tenv_final decs;
      (venv, tenv_final, exps)
    in
    List.fold_left transDec (venv, tenv, []) decs
and transExp ?(breakpoint=None) (venv: venv) (tenv: tenv) (level: Tr.level) (exp: A.exp) =
  let rec trexp (exp : A.exp) =
    match exp with
      | VarExp var -> trvar var
      | NilExp -> (Tr.nil_exp, Types.NIL)
      | IntExp i -> (Tr.int_exp i, Types.INT)
      | StringExp (str, pos) -> (Tr.string_exp str, Types.STRING)
      | CallExp { func; args; pos } ->
        (match Symbol.look func venv with
        | Some (E.FunEntry { level = def_level; label; formals; result }) -> 
          let arg_exps, arg_types = List.split (List.map trexp args) in
          if List.equal Types.(=) arg_types formals then
            (Tr.call_exp def_level level label arg_exps, result)
          else if List.(length arg_types = length formals) then
            error pos "mismatched argument type in function call"
          else 
            error pos "incorrect number of arguments in function call"
        | Some (E.VarEntry _) -> error pos "expected function; got variable"
        | None -> error pos "undefined function")
      | OpExp {left; oper; right; pos} ->
        let l_exp, l_type = trexp left in
        let r_exp, r_type = trexp right in
        (match oper with
          | PlusOp | MinusOp | TimesOp | DivideOp ->
              if all_type INT [l_type; r_type] then
                (Tr.arith_exp oper l_exp r_exp, Types.INT)
              else 
                error pos "arithmatic expects integers"
          | LtOp | LeOp | GtOp | GeOp ->
              if all_type INT [l_type; r_type] || all_type STRING [l_type; r_type] then
                (Tr.rel_exp oper l_exp r_exp, Types.INT)
              else 
                error pos "mismatched types on comparision"
          | EqOp | NeqOp ->
            match l_type, r_type with
            | INT, INT| STRING, STRING | ARRAY _, ARRAY _ | RECORD _, RECORD _ 
            | RECORD _, NIL | NIL, RECORD _ | NIL, NIL -> (Tr.rel_exp oper l_exp r_exp, Types.INT)
            | _ -> error pos "invalid eq comparison")
      | RecordExp {fields; typ; pos} ->
        (match (Option.map Types.actual_ty (Symbol.look typ tenv)) with
        | Some (RECORD (def_fields, id)) ->
          let typecheck_field (sym, exp, pos) =
            let field_exp, field_ty = trexp exp in
            (match List.assoc_opt sym def_fields with
            | Some ty -> if ty = field_ty then field_exp else error pos "mismatched field types"
            | _ -> error pos ("field " ^ Symbol.name sym ^ " not found in record type"))
          in
          let field_exps = List.map typecheck_field fields in 
          (Tr.record_exp field_exps, RECORD (def_fields, id))
        | Some _ -> error pos "expected record type, got other type"
        | None -> error pos ("undefined type: " ^ Symbol.name typ))
      | SeqExp seq -> 
        let seq_exps, seq_tys = List.split (List.map (fun (e, _) -> trexp e) seq) in
        (Tr.seq_exp seq_exps, match seq with [] -> UNIT | s -> List.(hd (rev seq_tys)))
      | AssignExp {var; exp; pos} -> 
        let var_exp, var_ty = trvar var in
        let exp_exp, exp_ty = trexp exp in
        if Types.(var_ty = exp_ty) then
          (Tr.assign_exp var_exp exp_exp, UNIT)
        else error pos "mismatched types in assignment"
      | IfExp {test; then'; else'; pos} ->
        let test_exp, test_ty = trexp test in
        let then_exp, then_ty = trexp then' in
        if Types.(test_ty = INT) then
          match else' with
          | None -> 
            if Types.(then_ty = UNIT) then
              (Tr.cond_exp test_exp then_exp None, UNIT)
            else error pos "if without else must return unit"
          | Some else_blk ->
            let else_exp, else_ty = trexp else_blk in
            if Types.(then_ty = else_ty) then
              (Tr.cond_exp test_exp then_exp (Some else_exp), then_ty)
            else error pos "mismatched type between then and else block"
        else error pos "type of condition is not int"
      | WhileExp {test; body; pos} ->
        let breakpoint = Temp.new_label () in
        let test_cond, test_ty = trexp test in
        if Types.(test_ty = INT) then
          let body_stm, body_ty = transExp ~breakpoint:(Some breakpoint) venv tenv level body in
          if Types.(body_ty = UNIT) then
            (Tr.while_exp breakpoint test_cond body_stm, UNIT)
          else error pos "while must return unit"
        else error pos "type of condition is not int"
      | ForExp {var; escape; lo; hi; body; pos} ->
        if not (all_type INT [get_type lo; get_type hi]) then error pos "low and high must both be integers";
        let i = A.SimpleVar (var, pos) in
        let limit = Symbol.symbol "$limit" in
        let typ = Some (Symbol.symbol "int", pos) in
        let decs = [ 
          A.VarDec { name = var; escape; typ; init = lo; pos }; 
          A.VarDec { name = limit; escape; typ; init = hi; pos }
        ] in
        let body = A.WhileExp {
          test = A.OpExp { left = A.VarExp i; oper = A.LeOp; right = A.VarExp (A.SimpleVar (limit, pos)); pos };
          body = A.SeqExp [ 
            (body, pos); 
            (A.AssignExp { var = i; exp = A.OpExp { left = A.VarExp i; oper = A.PlusOp; right = A.IntExp 1; pos }; pos }, pos) 
          ];
          pos
        } in
        trexp (A.LetExp { decs; body; pos })
      | BreakExp pos -> 
        (match breakpoint with
        | Some bp -> (Tr.break_exp bp, UNIT)
        | None -> error pos "break not in loop construct")
      | LetExp {decs; body; pos} ->
        let venv', tenv', exps = transDecs ~breakpoint venv tenv level decs in
        let body_exp, ty = transExp ~breakpoint venv' tenv' level body in
        (Tr.let_exp exps body_exp, ty)
      | ArrayExp {typ; size; init; pos} -> 
        (match (Option.map Types.actual_ty (Symbol.look typ tenv)) with
        | Some (ARRAY (internal_ty, id)) ->
          let size_exp, size_ty = trexp size in
          if size_ty != INT then error pos "size of array is not int";
          let init_exp, init_ty = trexp init in
          if Types.(init_ty = internal_ty) then
            (Tr.array_exp size_exp init_exp, ARRAY (internal_ty, id))
          else error pos "mismatched init type"
        | Some t -> error pos "expected array type, got other type"
        | None -> error pos ("undefined type: " ^ Symbol.name typ))
  and trvar = function
    | SimpleVar (sym, pos) ->
      (match Symbol.look sym venv with
        | Some(E.VarEntry { access; ty }) -> (Tr.simple_var access level, Types.actual_ty ty)
        | Some(E.FunEntry _) -> error pos "tried to access variable, got function"
        | None -> error pos ("undefined variable: " ^ Symbol.name sym))
    | FieldVar (var, sym, pos) ->
      let base, var_ty = trvar var in
      (match Types.actual_ty var_ty with
      | RECORD (fields, _) ->
        let rec find i = function
          | [] -> error pos "unknown field"
          | (sym', ty) :: rest -> if sym' = sym then (i, ty) else find (i + 1) rest
        in
        let i, ty = find 0 fields in
        (Tr.field_var base i, ty)
      | _ -> error pos "tried to access field of non record")
    | SubscriptVar (var, exp, pos) ->
      let base, var_ty = trvar var in
      (match var_ty with
      | ARRAY (ty, _) ->
        let (sub, ty) = trexp exp in
        if ty = INT then
          (Tr.subscript_var base sub, Types.actual_ty ty)
        else error pos "array index is not integer"
      | _ -> error pos "subscript used on nonarray type")

  and get_type exp =
    let (_, ty) = trexp exp in ty
  in trexp exp
and transTy tenv (ty : A.ty) =
  let look_ty sym pos = 
    match Symbol.look sym tenv with
    | Some t -> t
    | None -> error pos "undefined type" in
  match ty with
  | NameTy (sym, pos) -> look_ty sym pos
  | RecordTy fields ->
    let look_field ({name; typ; pos; _}: A.field) = (name, look_ty typ pos) in
    Types.RECORD ((List.map look_field fields), ref ())
  | ArrayTy (sym, pos) -> Types.ARRAY (look_ty sym pos, ref ())