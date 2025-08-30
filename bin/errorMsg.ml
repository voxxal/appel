let anyErrors = ref false
let fileName = ref ""
let lineNum = ref 1
let linePos = ref [1]
let source_stream = ref stdin

let reset () =
  anyErrors := false;
  fileName := "";
  lineNum := 1;
  linePos := [1];
  source_stream := stdin;

exception Error of string

let _error (pos: Lexing.position) msg =
  anyErrors := true;
  Printf.sprintf "%d:%d in %s: %s" pos.pos_lnum (pos.pos_cnum - pos.pos_bol) !fileName msg

let error pos msg =
  raise (Error (_error pos msg))


let impossible msg =
  Printf.printf "Error: Compiler Bug: %s\n" msg;
  flush stdout;
  raise (Error msg)