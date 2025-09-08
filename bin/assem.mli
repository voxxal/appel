type reg = string
type temp = Temp.t
type label = Temp.label

type instr =
  | OPER of { 
    assem: string;
    dst: temp list;
    src: temp list;
    jump: label list option
  }
  | LABEL of { assem: string; label: label }
  | MOVE of {
    assem: string;
    dst: temp;
    src: temp;
  }

val format: (temp -> string) -> instr -> string