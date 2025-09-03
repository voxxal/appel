type venv = Env.entry Symbol.table
type tenv = Types.t Symbol.table

type expty = Translate.exp * Types.t

let error = ErrorMsg.error
let impossible = ErrorMsg.impossible

module A = Absyn
module E = Env

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
  let (_, t) = transExp E.base_venv E.base_tenv Translate.outermost exp in
  ()
and transDec venv tenv level = function
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
        level = Translate.new_level level label (List.map (fun (f: A.field) -> !(f.escape)) params);
        label;
        formals = List.map (fun p -> let (_, t) = param_type p in t) params;
        result = res_type result 
      }) venv
    in
    let enter_param venv ((name, ty), access) = Symbol.enter name (E.VarEntry {access; ty}) venv in
    let process_fun venv ({name; params; result; body; pos;} : A.fundec) =
      let level = match Symbol.look name venv with Some(E.FunEntry {level; _}) -> level | _ -> impossible "function name changed?" in
      let params' = List.map param_type params in
      let body_venv = List.fold_left enter_param venv (List.combine params' (Translate.formals level)) in
      let (_, actual_res_type) = transExp body_venv tenv level body in
      if Types.(actual_res_type = res_type result) then
        venv
      else error pos "result type did not match expected result type"
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
    (venv_final, tenv)
  | VarDec {name; escape; typ = Some typ; init; pos} -> 
    let (_, init_ty) = transExp venv tenv level init in
    let (type_sym, _) = typ in
    let dec_type = Symbol.look type_sym tenv in
    let access = Translate.alloc_local level !escape in
    (match dec_type with
    | Some typ ->
      if Types.(init_ty = typ) then
        (Symbol.enter name (E.VarEntry {access; ty = init_ty}) venv, tenv)
      else error pos "type does not match expected type in var declaration"
    | None -> error pos "type in variable declaration is undefined")
  | VarDec {name; escape; typ = None; init; pos} -> 
    let (_, ty) = transExp venv tenv level init in
    let access = Translate.alloc_local level !escape in
    if ty = Types.NIL then
      error pos "variable initalized to nil with no type defined"
    else
      (Symbol.enter name (E.VarEntry {access; ty}) venv, tenv)
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
    (venv, tenv_final)
and transExp ?(breakable=false) (venv: venv) (tenv: tenv) (level: Translate.level) (exp: A.exp) =
  let rec trexp (exp : A.exp) =
    match exp with
      | VarExp var -> trvar var
      | NilExp -> ((), Types.NIL)
      | IntExp _ -> ((), Types.INT)
      | StringExp _ -> ((), Types.STRING)
      | CallExp { func; args; pos } ->
        (match (Symbol.look func venv) with
        | Some (E.FunEntry {formals; result; _}) -> 
          let arg_types = List.map get_type args in
          if List.equal Types.(=) arg_types formals then
            ((), result)
          else if List.(length arg_types = length formals) then
            error pos "mismatched argument type in function call"
          else 
            error pos "incorrect number of arguments in function call"
        | Some (E.VarEntry _) -> error pos "expected function; got variable"
        | None -> error pos "undefined function")
      | OpExp {left; oper; right; pos} ->
        let l_type = get_type left in
        let r_type = get_type right in
        (match oper with
          | PlusOp | MinusOp | TimesOp | DivideOp ->
              if all_type INT [l_type; r_type] then
                ((), Types.INT)
              else 
                error pos "arithmatic expects integers"
          | LtOp | LeOp | GtOp | GeOp ->
              if all_type INT [l_type; r_type] || all_type STRING [l_type; r_type] then
                ((), Types.INT)
              else 
                error pos "mismatched types on comparision"
          | EqOp | NeqOp ->
            match l_type, r_type with
            | INT, INT| STRING, STRING | ARRAY _, ARRAY _ | RECORD _, RECORD _ 
            | RECORD _, NIL | NIL, RECORD _ | NIL, NIL -> ((), Types.INT)
            | _ -> error pos "invalid eq comparison")
      | RecordExp {fields; typ; pos} ->
        (match (Option.map Types.actual_ty (Symbol.look typ tenv)) with
        | Some (RECORD (def_fields, id)) ->
          let field_eq (def_sym, def_ty) (user_sym, user_ty) =
               def_sym = user_sym && Types.(def_ty = user_ty)
          in
          let resolved_user_fields = List.map (fun (s, e, _) -> (s, get_type e)) fields in
          if List.equal field_eq def_fields resolved_user_fields then
            ((), RECORD (def_fields, id))
          else
            error pos "mismatched field types"
        | Some _ -> error pos "expected record type, got other type"
        | None -> error pos ("undefined type: " ^ Symbol.name typ))
      | SeqExp seq -> 
        (match seq with 
        | [] -> ((), UNIT)
        (* may god forgive me for my sins *)
        | s -> ((), List.(hd (rev (map (fun (e, _) -> get_type e) s)))))
      | AssignExp {var; exp; pos} -> 
        let (_, var_ty) = trvar var in
        let exp_ty = get_type exp in
        if Types.(var_ty = exp_ty) then
          ((), UNIT)
        else error pos "mismatched types in assignment"
      | IfExp {test; then'; else'; pos} ->
        let then_ty = get_type then' in
        if Types.(get_type test = INT) then
          match else' with
          | None -> 
            if Types.(then_ty = UNIT) then
              ((), UNIT)
            else error pos "if without else must return unit"
          | Some else_blk ->
            if Types.(then_ty = get_type else_blk) then
              ((), then_ty)
            else error pos "mismatched type between then and else block"
        else error pos "type of condition is not int"
      | WhileExp {test; body; pos} ->
        if Types.(get_type test = INT) then
          let (_, body_ty) = transExp ~breakable:true venv tenv level body in
          if Types.(body_ty = UNIT) then
            ((), UNIT)
          else error pos "while must return unit"
        else error pos "type of condition is not int"
      | ForExp {var; escape; lo; hi; body; pos} ->
        if all_type INT [get_type lo; get_type hi] then
         let for_venv = Symbol.enter var (E.VarEntry { access = Translate.alloc_local level !escape; ty = INT }) venv in
         let (_, body_ty) = transExp ~breakable:true for_venv tenv level body in
          if Types.(body_ty = UNIT) then
            ((), UNIT)
          else error pos "for statement body must return unit"
        else error pos "low and high must both be integers"
      | BreakExp pos -> 
        if breakable = true then 
          ((), UNIT) 
        else 
          error pos "break not in loop construct"
      | LetExp {decs; body; pos} -> 
        let (venv', tenv') = List.fold_left (fun (venv, tenv) dec -> transDec venv tenv level dec) (venv, tenv) decs in
        transExp venv' tenv' level body
      | ArrayExp {typ; size; init; pos} -> 
        (match (Option.map Types.actual_ty (Symbol.look typ tenv)) with
        | Some (ARRAY (internal_ty, id)) ->
          if Types.(get_type init = internal_ty) then
            ((), ARRAY (internal_ty, id))
          else error pos "mismatched init type"
        | Some t -> error pos "expected array type, got other type"
        | None -> error pos ("undefined type: " ^ Symbol.name typ))
  and trvar = function
    | SimpleVar (sym, pos) ->
      (match Symbol.look sym venv with
        | Some(E.VarEntry {ty; _}) -> ((), Types.actual_ty ty)
        | Some(E.FunEntry _) -> error pos "tried to access variable, got function"
        | None -> error pos ("undefined variable: " ^ Symbol.name sym))
    | FieldVar (var, sym, pos) ->
      let (_, var_ty) = trvar var in
      (match var_ty with
      | RECORD (fields, _) ->
        (match List.find_opt (fun (f, _) -> f = sym) fields with
        | Some (_, ty) -> ((), Types.actual_ty ty)
        | None -> error pos "record does not exist in field")
      | _ -> error pos "tried to access field of non record")
    | SubscriptVar (var, exp, pos) ->
      let (_, var_ty) = trvar var in
      (match var_ty with
      | ARRAY (ty, _) ->
        if get_type exp = INT then
          ((), Types.actual_ty ty)
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