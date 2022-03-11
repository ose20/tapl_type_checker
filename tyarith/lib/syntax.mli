open Support.Pervasive
open Support.Error

type ty =
  | TyBool
  | TyNat

type term =
  | TmTrue of info
  | TmFalse of info
  | TmIf of info * term * term * term
  | TmZero of info
  | TmSucc of info * term
  | TmPred of info * term
  | TmIsZero of info * term

type command =
  | Import of string
  | Eval of info * term

val tmInfo : term -> info
val printty : ty -> unit
val printtm : term -> unit