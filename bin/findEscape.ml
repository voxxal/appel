type depth = int
type esc_env = (depth * bool ref) Symbol.table

let error = ErrorMsg.error

module A = Absyn

let rec traverse_exp env depth = function
  | A.VarExp var -> traverse_var env depth var
  | NilExp | IntExp _ | StringExp _ | BreakExp _ -> ()
  | CallExp { func; args; pos } -> 
    List.iter (fun arg -> traverse_exp env depth arg) args
  | OpExp { left; right; _ } ->
    traverse_exp env depth left;
    traverse_exp env depth right;
  | RecordExp { fields; _ } ->
    List.iter (fun (_, exp, _) -> traverse_exp env depth exp) fields
  | SeqExp exps -> 
    List.iter (fun (exp, _) ->  traverse_exp env depth exp) exps
  | AssignExp { var; exp; _} ->
    traverse_var env depth var;
    traverse_exp env depth exp
  | IfExp { test; then'; else'; _ } ->
    traverse_exp env depth test;
    traverse_exp env depth then';
    (match else' with Some exp -> traverse_exp env depth exp | None -> ())
  | WhileExp { test; body; _ } ->
    traverse_exp env depth test;
    traverse_exp env depth body
  | ForExp { var; escape; lo; hi; body; _ } ->
    let env = Symbol.enter var (depth, escape) env in
    escape := false;
    traverse_exp env depth lo;
    traverse_exp env depth hi;
    traverse_exp env depth body
  | LetExp { decs; body; _ } ->
    let env = traverse_decs env depth decs in
    traverse_exp env depth body
  | ArrayExp { size; init; _ } ->
    traverse_exp env depth size;
    traverse_exp env depth init;
and traverse_decs env depth decs =
  let traverse_dec env = function
    | A.VarDec { name; escape; _ } -> escape := false; Symbol.enter name (depth, escape) env
    | FunctionDec fn_decs -> 
      let fn_depth = depth + 1 in
      let traverse_field env ({ name; escape; _ }: A.field) = escape := false; Symbol.enter name (fn_depth, escape) env in
      let single_dec ({ params; body; _ }: A.fundec) = 
        let func_env = List.fold_left traverse_field env params in
        traverse_exp func_env fn_depth body
      in
      List.iter single_dec fn_decs;
      env
    | TypeDec _ -> env
    in 
    List.fold_left traverse_dec env decs
and traverse_var env depth = function
    | A.SimpleVar (name, pos) ->
      (match Symbol.look name env with 
        | Some (var_depth, var_escape) -> if depth > var_depth then var_escape := true
        | None -> error pos ("undefined variable: " ^ Symbol.name name))
    | FieldVar (base_var, _, _) -> traverse_var env depth base_var
    | SubscriptVar (base_var, index, _) ->
        traverse_exp env depth index;
        traverse_var env depth base_var

let find_escape prog = traverse_exp Symbol.empty 0 prog