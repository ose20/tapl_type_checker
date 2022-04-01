open Support.Error

type ty =
  | Ty_arr of ty * ty
  | Ty_bool

type term =
  | Tm_var    of info * int * int (* first int means its nemeless term, and second one means the length of the context in which this variable appears *)
  | Tm_abs    of info * string * ty * term
  | Tm_app    of info * term * term
  | Tm_letin  of info * string * term * term
  | Tm_true   of info
  | Tm_false  of info
  | Tm_if     of info * term * term * term

type binding =
  | Name_bind
  | Var_bind of ty

type command =
  | Eval of info * term

type context
val info_of_term : term -> info
(* ----- Context management ----- *)
val empty_context : context
val ctx_len       : context -> int
val add_binding   : context -> string -> binding -> context
val add_name      : context -> string -> context
val name2index    : info -> context -> string -> int
(* ----- Evaluation ----- *)
exception No_rule_applies

val eval : term -> term
(* ----- Printing ----- *)
val printty     : ty -> unit
val printtm     : context -> term -> unit
val print_tmty  : context -> term -> unit