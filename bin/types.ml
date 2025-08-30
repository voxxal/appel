type t = 
    | RECORD of (Symbol.t * t) list * unique
    | NIL
    | INT
    | STRING
    | ARRAY of t * unique
	| NAME of Symbol.t * t option ref
    | UNIT
and unique = unit ref

let rec actual_ty (ty: t) =
    match ty with
    | NAME (_, wrap_ty) ->
        (match !wrap_ty with
        | Some real_ty -> actual_ty real_ty
        | None -> ty)
    | _ -> ty


let ( = ) (a: t) (b: t) =
    match (actual_ty a), (actual_ty b) with
    | RECORD (_, id_a), RECORD (_, id_b) ->
        id_a == id_b
    | ARRAY (_, id_a), ARRAY (_, id_b) ->
        id_a == id_b
    | (RECORD _, NIL) | (NIL, RECORD _) -> true
    | NAME (id_a, _), NAME (id_b, _) -> (Symbol.name id_a) = (Symbol.name id_b)
    | a_other, b_other -> Stdlib.(a_other = b_other)  

let rec format = function
    | RECORD (fields, _) ->
        let format_field (sym, t) = "\t" ^ Symbol.name sym ^ ": " ^  format t in
        let field_text = String.concat "\n" (List.map format_field fields) in
        "{\n" ^ field_text ^ "\n}"
    | NIL -> "Nil"
    | INT -> "Int"
    | STRING -> "String"
    | ARRAY (ty, _) -> (format ty) ^ "[]"
    | NAME (name, actual) -> 
        (match !actual with 
        | Some ty -> format ty ^ " (" ^ Symbol.name name ^ ")" 
        | None -> "<type hole> (" ^ Symbol.name name ^ ")")
    | UNIT -> "Unit"