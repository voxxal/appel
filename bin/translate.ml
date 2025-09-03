type exp = unit
(* prev frame / current frame *)
type level = Frame.t * Frame.t
type access = level * Frame.access

let curr_frame (_, frame) = frame

let outermost =
  let outer = Frame.new_frame (Temp.named_label "outermost") [] in
    outer, outer

let new_level (level: level) name formals: level = 
  curr_frame level, Frame.new_frame name (true :: formals)
let formals (level: level) = 
  let formals_no_sl = match Frame.formals (curr_frame level) with [] -> [] | sl :: rest -> rest in
  List.map (fun acc -> level, acc) formals_no_sl
let alloc_local (level: level) escape = level, Frame.alloc_local (curr_frame level) escape