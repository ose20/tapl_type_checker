open Syntax
open Support.Error

(* -------------------- *)
(* evaluation *)

exception No_rule_applies

val eval : context -> term -> term

(* -------------------- *)
(* typing *)

type constr = (ty * ty) list

val empty_constr    : constr 
val combine_constr  : constr -> constr -> constr
val print_constr    : constr -> unit

type next_uvar      = Next_uvar of string * uvar_generator
and uvar_generator  = unit -> next_uvar

val uvar_gen : uvar_generator

val recon : context -> uvar_generator -> term -> (ty * uvar_generator * constr)

val type_subst  : ty -> ty -> ty -> ty
val apply_subst : (ty * ty) list -> ty -> ty

val unify : info -> string -> constr -> (ty * ty) list

