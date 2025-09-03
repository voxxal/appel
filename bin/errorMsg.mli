val anyErrors : bool ref
val fileName : string ref
val source_stream : in_channel ref
val error : Lexing.position -> string -> 'a (* raises Error *)
exception Error of string
val impossible : string -> 'a   (* raises Error *)
val reset : unit -> unit