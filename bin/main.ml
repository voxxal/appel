let print_token : Parser.token -> string =
  fun _tok ->
    match _tok with
    | AND ->
        "AND"
    | ARRAY ->
        "ARRAY"
    | ASSIGN ->
        "ASSIGN"
    | BREAK ->
        "BREAK"
    | COLON ->
        "COLON"
    | COMMA ->
        "COMMA"
    | DIVIDE ->
        "DIVIDE"
    | DO ->
        "DO"
    | DOT ->
        "DOT"
    | ELSE ->
        "ELSE"
    | END ->
        "END"
    | EOF ->
        "EOF"
    | EQ ->
        "EQ"
    | FOR ->
        "FOR"
    | FUNCTION ->
        "FUNCTION"
    | GE ->
        "GE"
    | GT ->
        "GT"
    | ID a ->
        "ID " ^ a
    | IF ->
        "IF"
    | IN ->
        "IN"
    | INT a ->
        "INT " ^ (string_of_int a)
    | LBRACE ->
        "LBRACE"
    | LBRACK ->
        "LBRACK"
    | LE ->
        "LE"
    | LET ->
        "LET"
    | LPAREN ->
        "LPAREN"
    | LT ->
        "LT"
    | MINUS ->
        "MINUS"
    | NEQ ->
        "NEQ"
    | NIL ->
        "NIL"
    | OF ->
        "OF"
    | OR ->
        "OR"
    | PLUS ->
        "PLUS"
    | RBRACE ->
        "RBRACE"
    | RBRACK ->
        "RBRACK"
    | RPAREN ->
        "RPAREN"
    | SEMICOLON ->
        "SEMICOLON"
    | STRING a ->
        "STRING " ^ a
    | THEN ->
        "THEN"
    | TIMES ->
        "TIMES"
    | TO ->
        "TO"
    | TYPE ->
        "TYPE"
    | VAR ->
        "VAR"
    | WHILE ->
        "WHILE"


let lex_driver channel =
  let lexbuf = Lexing.from_channel ~with_positions: true channel in
  let rec lex () =
    let tok = Lexer.token lexbuf in
    print_endline (print_token tok);
    if tok = Parser.EOF then
       () 
    else 
      lex () in
  lex ()


let parse_driver channel =
    let lexbuf = Lexing.from_channel channel in
    Prabsyn.print stdout (Parser.program Lexer.token lexbuf)

let semant_driver channel =
  let lexbuf = Lexing.from_channel channel in
  let parsed = Parser.program Lexer.token lexbuf in
  let (_, t) = Semant.transExp Env.base_venv Env.base_tenv parsed in
  (* print_endline t *)
  ()


let () = 
  let filename = Sys.argv.(1) in
  let file = open_in filename in
    ErrorMsg.fileName := filename;
    print_endline "%% lex step";
    lex_driver file;
    print_endline "%% parse step";
    seek_in file 0;
    ignore (parse_driver file);
    print_endline "%% semant step";
    seek_in file 0;
    ignore (semant_driver file);
    close_in file