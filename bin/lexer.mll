{
    open Parser
    let string_buf = Buffer.create 256
    let depth = ref 0
}

(* https://stackoverflow.com/questions/66307896/lexing-strings-in-ocamllex *)

let ws = [' ''\t']

let digit = ['0'-'9']
let letter = ['a'-'z'] | ['A'-'Z']

rule token = parse
    | eof { EOF }
    | '\n' { Lexing.new_line lexbuf; token lexbuf }
    | ws { token lexbuf }
    | "while" {WHILE}
    | "for" {FOR}
    | "to" {TO}
    | "break" {BREAK}
    | "let" {LET}
    | "in" {IN}
    | "end" {END}
    | "function" {FUNCTION}
    | "var" {VAR}
    | "type" {TYPE}
    | "array" {ARRAY}
    | "if" {IF}
    | "then" {THEN}
    | "else" {ELSE}
    | "do" {DO}
    | "of" {OF}
    | "nil" {NIL}
    | ":=" {ASSIGN}
    | "." {DOT}
    | "{" {LBRACE}
    | "}" {RBRACE}
    | "[" {LBRACK}
    | "]" {RBRACK}
    | "(" {LPAREN}
    | ")" {RPAREN}
    | ";" {SEMICOLON}
    | ":" {COLON}
    | "," {COMMA}
    | "+" {PLUS}
    | "-" {MINUS}
    | "*" {TIMES}
    | "/" {DIVIDE}
    | "<>" {NEQ}
    | "=" {EQ}
    | "<" {LT}
    | "<=" {LE}
    | ">" {GT}
    | ">=" {GE}
    | "&" {AND}
    | "|" {OR}
    | digit+ as int {INT (int_of_string int)}
    | letter (digit | letter | '_')* as id {ID (id)}
    | '"' { Buffer.clear string_buf; string lexbuf; STRING (Buffer.contents string_buf) }
    | "/*" { depth := 1; comment lexbuf}
    and string = parse
    | '"' { () }
    | '\n' { Lexing.new_line lexbuf; Buffer.add_char string_buf '\n'; string lexbuf }
    | "\\t" { Buffer.add_char string_buf '\t'; string lexbuf }
    | "\\n" { Buffer.add_char string_buf '\n'; string lexbuf }
    | "\\\\" { Buffer.add_char string_buf '\\'; string lexbuf }
    | "\\\"" { Buffer.add_char string_buf '"'; string lexbuf }
    | eof { ErrorMsg.error (Lexing.lexeme_start_p lexbuf) "unclosed string" }
    | _ as c { Buffer.add_char string_buf c; string lexbuf }
    and comment = parse
    | "/*" { depth := !depth + 1; comment lexbuf }
    | "*/" { depth := !depth - 1; 
                if !depth > 0 then
                    comment lexbuf 
                else if !depth = 0 then
                    token lexbuf 
                else 
                    ErrorMsg.error (Lexing.lexeme_start_p lexbuf) "unclosed comment"; }
    | '\n' { Lexing.new_line lexbuf; comment lexbuf }
    | _ { comment lexbuf }
    | eof { ErrorMsg.error (Lexing.lexeme_start_p lexbuf) "unclosed comment";}