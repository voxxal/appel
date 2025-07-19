%{
    open Ast
%}

%token EOF
%token <string> ID
%token <int> INT
%token <string> STRING
%token RBRACE LBRACE RBRACK LBRACK RPAREN LPAREN SEMICOLON COLON COMMA
%token WHILE FOR TO BREAK LET IN END FUNCTION VAR TYPE ARRAY IF THEN ELSE DO OF NIL
%token ASSIGN DOT
%token PLUS MINUS TIMES DIVIDE NEQ EQ LT LE GT GE
%token AND OR

%start <Ast.exp> program

%nonassoc ASSIGN

%right OF
%right DO THEN ELSE

%left OR
%left AND
%nonassoc EQ NEQ LT LE GT GE
%left PLUS MINUS
%left TIMES DIVIDE
%nonassoc UMINUS


%%

let program := 
    | exp = exp; EOF; { exp }

let exp :=
    | lvalue; { UnimplExp }
    // valueless?
    | NIL; { NilExp }
    | LPAREN; separated_list(SEMICOLON, exp); RPAREN; { UnimplExp }
    | INT; { UnimplExp }
    | STRING; { UnimplExp }
    | MINUS; exp; { UnimplExp } %prec UMINUS
    | ID; LPAREN; separated_list(COMMA, exp); RPAREN; { UnimplExp }
    | exp; bin_op; exp; { UnimplExp }
    | type_id; LBRACE; separated_list(COMMA, rec_field); RBRACE; { UnimplExp }
    // NOTE semantically, this shouldn't be possible but i cant figure out a good way of correcting the grammar to fix this.
    | lvalue; OF; exp; { UnimplExp }
    | lvalue; ASSIGN; exp; { UnimplExp }
    | IF; exp; THEN; exp; ELSE; exp; { UnimplExp }
    | IF; exp; THEN; exp; { UnimplExp }
    | WHILE; exp; DO; exp; { UnimplExp }
    | FOR; ID; ASSIGN; exp; TO; exp; DO; exp; { UnimplExp }
    | BREAK; { UnimplExp }
    | LET; decs; IN; separated_list(SEMICOLON, exp); END; { UnimplExp }

let rec_field := ID; EQ; exp; <>

let bin_op ==
    // arith
    | PLUS; <>
    | MINUS; <>
    | TIMES; <>
    | DIVIDE; <>
    // comp
    | EQ; <>
    | NEQ; <>
    | GT; <>
    | LT; <>
    | GE; <>
    | LE; <>
    // bool
    | AND; <>
    | OR; <>

// Data Types
let tydec := TYPE; type_id; EQ; ty; { TyDec }
let ty :=
    | type_id; { Ty [] }
    | LBRACE; fields = tyfields; RBRACE; { Ty fields }
    | ARRAY; OF; type_id; { Ty [] }
let tyfields := separated_list(COMMA, tyfield); { [] }
let tyfield := ID; COLON; type_id; <>


// Declarations
let decs := list(dec); <>
let dec :=
    | tydec; <>
    | vardec; <>
    | fundec; <>

let vardec :=
    | VAR; ID; ASSIGN; exp; { VarDec }
    | VAR; ID; COLON; type_id; ASSIGN; exp; { VarDec }

let fundec :=
    | FUNCTION; ID; LPAREN; tyfields; RPAREN; EQ; exp; { FunDec }
    | FUNCTION; ID; LPAREN; tyfields; RPAREN; COLON; type_id; EQ; exp; { FunDec }

let lvalue :=
    | ID; <>
    | lvalue; DOT; ID; <>
    | lvalue; LBRACK; exp; RBRACK; <>

let type_id == id = ID; { id }