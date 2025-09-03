(* TODO figure out a better way of structuring this *)
module type Frame = sig
  type t
  type access

  val new_frame: Temp.label -> bool list -> t
  val formals: t -> access list
  val alloc_local: t -> bool -> access
end

module FrameRiscV : Frame = struct 
  type access = InFrame of int | InReg of Temp.t
  type t = { formals: access list; locals: int ref; label: Temp.label } 

  (* rv64 *)
  let word_size = 8
  (* a0 - a7 *)
  let max_args_in_regs = 8

  let alloc_formals (acc, offset, in_regs) escape = 
    if escape || in_regs >= max_args_in_regs then
      (InFrame offset :: acc, offset + word_size, in_regs)
    else 
      (InReg (Temp.new_temp ()) :: acc, offset, in_regs + 1)

  let new_frame label formals = 
    let (formals, _, _) = List.fold_left alloc_formals ([], 0, 0) formals in
    { formals; label; locals = ref 0 }
  let formals frame = frame.formals

  let alloc_local {locals;_} escape =
    if escape then 
      (locals := !locals + 1; InFrame (!locals * word_size))
    else
      InReg (Temp.new_temp ())
end

include FrameRiscV