open Format
open Support.Error
open Support.Pervasive

(* --------------------------------------------------------------------------------- *)
(* Datetypes *)
type term =
	| TmVar of info * int * int
	| TmAbs of info * string * term
	| TmApp of info * term * term

type command =
	| Import of string
	| Eval of info * term
	| Bind of info * string

(* --------------------------------------------------------------------------------- *)
(* Context management *)

type context
val emptycontext : context
val ctxlen : context -> int
val addname : context -> string -> context
val name2index : info -> context -> string -> int
val index2name : info -> context -> int -> string

(* --------------------------------------------------------------------------------- *)
(* Shifting & Substitution *)

val shiftTerm : int -> term -> term
(* beta-redution of the term (\lambda. t)v *)
val betaReduction : term -> term -> term

(* --------------------------------------------------------------------------------- *)
(* Extracting file info *)

val info_of_term : term -> info

(* --------------------------------------------------------------------------------- *)
(* Printing *)

val printtm : context -> term -> unit