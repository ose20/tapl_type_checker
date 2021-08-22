open Support.Error
open Support.Pervasive

type term =
  | TmTrue of info
  | TmFalse of info
  | TmIf of info * term * term * term
  | TmZero of info
  | TmSucc of info * term
  | TmPred of info * term
  | TmIsZero of info * term

type command =
  | Eval of info * string
  | Import of string

val info_of_tm : term -> info

val printtm : term -> unit