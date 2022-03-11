module Pervasive : sig
  val pr : string -> unit
end

module Error : sig
  exception Exit of int

  type info
  val dummyinfo : info

  val createInfo : string -> int -> int -> info
  val printInfo : info -> unit

  type 'a withinfo = {i : info; v : 'a}

  val errf : (unit -> unit) -> 'a
  val errfAt : info -> (unit -> unit) -> 'a

  val err : string -> 'a
  val error : info -> string -> 'a

  val warning : string -> unit
  val warningAt : info -> string -> unit
end