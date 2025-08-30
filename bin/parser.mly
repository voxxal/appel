%{
    open Absyn
    open Symbol
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

%start <Absyn.exp> program
%type <Absyn.exp> exp

%nonassoc ASSIGN

%nonassoc OF
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
    | var = lvalue; { VarExp var }
    // valueless?
    | NIL; { NilExp }
    | LPAREN; list = separated_list(SEMICOLON, seqexp_item); RPAREN; { SeqExp list }
    | num = INT; { IntExp num }
    | str = STRING; { StringExp (str, $startpos) }
    | MINUS; exp = exp; { 
        OpExp {
            left = IntExp 0;
            oper = MinusOp;
            right = exp;
            pos = $startpos
        } 
    } %prec UMINUS
    | id = ID; LPAREN; args = separated_list(COMMA, exp); RPAREN; { 
        CallExp { func = symbol id; args; pos = $startpos } 
    }
    | left = exp; oper = bin_op; right = exp; { 
        OpExp {
            left;
            oper;
            right;
            pos = $startpos;
        }
    }
    // special handling of boolean operations because we interpret them as if exps
    | op = bool_op; <>
    | typ = type_id; LBRACE; fields = separated_list(COMMA, rec_field); RBRACE; { RecordExp { fields; typ = symbol typ; pos = $startpos } }
    | typ = type_id; LBRACK; size = exp; RBRACK; OF; init = exp; { ArrayExp { typ = symbol typ; size; init; pos = $startpos } }
    | var = lvalue; ASSIGN; exp = exp; { AssignExp { var; exp; pos = $startpos; } }
    | IF; test = exp; THEN; then_blk = exp; ELSE; else_blk = exp; {
        IfExp {
            test;
            then' = then_blk;
            else' = Some else_blk;
            pos = $startpos;
        }
    }
    | IF; test = exp; THEN; then_blk = exp; {
        IfExp {
            test;
            then' = then_blk;
            else' = None;
            pos = $startpos;
        }
    }
    | WHILE; test = exp; DO; body = exp; {
        WhileExp {
            test;
            body;
            pos = $startpos;
        }
    }
    | FOR; id = ID; ASSIGN; lo = exp; TO; hi = exp; DO; body = exp; {
        ForExp {
            var = symbol id;
            escape = ref true;
            lo;
            hi;
            body;
            pos = $startpos;
        }
    }
    | BREAK; { BreakExp $startpos }
    | LET; decs = decs; IN; body = separated_list(SEMICOLON, seqexp_item); END; {
        LetExp { decs; body = SeqExp (body); pos = $startpos }
    }

let seqexp_item == exp = exp; { (exp, $startpos) }

let rec_field := id = ID; EQ; exp = exp; { (symbol id, exp, $startpos) }

let bin_op ==
    // arith
    | PLUS; { PlusOp }
    | MINUS; { MinusOp }
    | TIMES; { TimesOp }
    | DIVIDE; { DivideOp }
    // comp
    | EQ; { EqOp }
    | NEQ; { NeqOp }
    | LT; { LtOp }
    | LE; { LeOp }
    | GT; { GtOp }
    | GE; { GeOp }

let bool_op ==
    | left = exp; AND; right = exp; { IfExp { test = left; then' = right; else' = Some (IntExp (0)); pos = $startpos } }
    | left = exp; OR; right = exp; { IfExp { test = left; then' = IntExp (1); else' = Some right; pos = $startpos } }

let tydec := TYPE; name = type_id; EQ; ty = ty; { { name = symbol name; ty; pos = $startpos } }
let ty :=
    | id = type_id; { NameTy (symbol id, $startpos) }
    | LBRACE; fields = tyfields; RBRACE; { RecordTy (fields) }
    | ARRAY; OF; id = type_id; { ArrayTy (symbol id, $startpos) }
let tyfields := fields = separated_list(COMMA, tyfield); <>
let tyfield := id = ID; COLON; typ = type_id; { { name = symbol id; escape = ref true; typ = symbol typ; pos = $startpos }}

let decs := decs = list(dec); <>
// this does cause a shift-reduce conflict, but it isn't of any worry because choosing to shift continues the tydec or fundec instead of ending it early.
let dec :=
    | typedecs = nonempty_list(tydec); { TypeDec typedecs }
    | dec = vardec; <>
    | funcdecs = nonempty_list(fundec); { FunctionDec funcdecs }

let vardec :=
    | VAR; name = ID; ASSIGN; init = exp; { 
        VarDec {
            name = symbol name;
            escape = ref true;
            typ = None; 
            init;
            pos = $startpos
        }
    }
    | VAR; name = ID; COLON; typ = type_id; ASSIGN; init = exp; { 
        VarDec {
            name = symbol name;
            escape = ref true;
            typ = Some (symbol typ, $startpos(typ));
            init;
            pos = $startpos
        }
    }

let fundec :=
    | FUNCTION; name = ID; LPAREN; params = tyfields; RPAREN; EQ; body = exp; {
        {
            name = symbol name;
            params;
            result = None;
            body;
            pos = $startpos
        }
    }
    | FUNCTION; name = ID; LPAREN; params = tyfields; RPAREN; COLON; res = type_id; EQ; body = exp; {
        {
            name = symbol name;
            params;
            result = Some (symbol res, $startpos(res));
            body;
            pos = $startpos
        }
    }

let lvalue :=
    | id = ID; { SimpleVar (symbol id, $startpos) }
    | var = lvalue; DOT; sub = ID; { FieldVar (var, symbol sub, $startpos)}
    | var = lvalue; LBRACK; exp = exp; RBRACK; { SubscriptVar (var, exp, $startpos)}
    // Redudnant rule to disambiguate from array creation
    | var = ID; LBRACK; exp = exp; RBRACK; { 
        SubscriptVar (
            SimpleVar (Symbol.symbol var, $startpos),
            exp,
            $startpos
        )
    }

let type_id == id = ID; { id }