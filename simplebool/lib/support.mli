module Pervasive : sig
  val pr : string -> unit
  val space : unit -> unit
  val break : int -> int -> unit
end

module Error : sig
  exception Exit of int
  type info
  val dummy_info : info

  val create_info : string -> int -> int -> info
  val print_info : info -> unit

  type 'a withinfo = {i : info; v : 'a}

  val errf : (unit -> unit) -> 'a
  val errf_at : info -> (unit -> unit) -> 'a

  val errs : string -> 'a
  val errs_at : info -> string -> 'a

  val warning : string -> unit
  val warning_at : info -> string -> unit
end