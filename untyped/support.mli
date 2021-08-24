open Format

module Error : sig
  
  exception Exit of int
  type info
  type 'a withinfo = {i : info; v : 'a}

  val dummyinfo : info
  val createInfo : string -> int -> int -> info
  val printInfo : info -> unit

  val errf : (unit -> unit) -> 'a
  val errfAt : info -> (unit -> unit) -> 'a
  val errs : string -> 'a
  val errsAt : info -> string -> 'a

  val warning : string -> unit
  val warningAt : info -> string -> unit

end

(* ----------------------------------------------------------------- *)

module Pervasive : sig

  val pr : string -> unit
  val hovbox : int -> unit
  val vbox : int -> unit
  val cbox : unit -> unit
  val break : int -> int -> unit
  val cut : unit -> unit
  val space : unit -> unit
  val newline : unit -> unit
  
end