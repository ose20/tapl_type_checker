open Support.Error

type ty =
  | Ty_arr of ty * ty
  | Ty_id of string
  | Ty_bool
  | Ty_nat

type term =
  | Tm_var    of info * int * int (* first int means its nemeless term, and second one means the length of the context in which this variable appears *)
  | Tm_abs    of info * string * ty option * term
  | Tm_app    of info * term * term
  | Tm_true   of info
  | Tm_false  of info
  | Tm_if     of info * term * term * term
  | Tm_zero   of info
  | Tm_succ   of info * term
  | Tm_pred   of info * term
  | Tm_iszero of info * term
  | Tm_let    of info * string * term * term

(* Data type to represent the type of the variable *)
type binding =
  | Name_bind
  | Var_bind of ty

(* list consistiong of pairs of variable name and its type *)
type context = (string * binding) list

type command = 
  | Eval of info * term

val empty_context  : context
val ctx_len       : context -> int

val add_binding : context -> string -> binding -> context
val add_name    : context -> string -> context

val index2name : info -> context -> int -> string
val name2index : info -> context -> string -> int

val get_type : info -> context -> int -> ty

val b_reduce : term -> term -> term

val info_of_term : term -> info

val printty : ty -> unit
val printtm : context -> term -> unit