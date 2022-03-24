open Support.Error

type ty =
  | TyArr of ty * ty
  | TyBool

type term =
  | TmVar of info * int * int
  | TmAbs of info * string * ty * term
  | TmApp of info * term * term
  | TmTrue of info
  | TmFalse of info
  | TmIf of info * term * term * term

type binding =
  | NameBind
  | VarBind of ty

type command = 
  | Import of string
  | Eval of info * term
  | Bind of info * string * binding


type context
val empty_context : context
val ctx_len : context -> int
val add_binding : context -> string -> binding -> context
val add_name : context -> string -> context
val is_name_bound : context -> string -> bool
val pick_fresh_name : context -> string -> context * string
val index2name : info -> context -> int -> string
val name2index : info -> context -> string -> int
val get_binding : info -> context -> int -> binding
val get_type_from_context : info -> context -> int -> ty
val info_of_term : term -> info

val printty : ty -> unit
val printtm : context -> term -> unit
val prbinding : binding -> unit


(* evaluation *)
exception NoRuleApplies

val eval : term -> term
val typeof : context -> term -> ty