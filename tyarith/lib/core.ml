open Syntax
open Support.Error

exception NoRuleApplies

let rec isnumericval = function
  | TmZero _ -> true
  | TmSucc (_, t1) -> isnumericval t1
  | _ -> false

let rec isval = function
  | TmTrue _ -> true
  | TmFalse _ -> true
  | t when isnumericval t -> true
  | _ -> false

(* 1-step evaluation *)
let rec eval1 = function
  | TmIf (_, TmTrue(_), t2, t3) -> t2
  | TmIf (_, TmFalse(_), t2, t3) -> t3
  | TmIf (fi, t1, t2, t3) ->
      let t1' = eval1 t1 in
      TmIf(fi, t1', t2, t3)
  | TmSucc (fi, t1) ->
      let t1' = eval1 t1 in TmSucc (fi, t1')
  | TmPred (_, TmZero(_)) -> TmZero(dummyinfo)
  | TmPred (_, TmSucc(_, nv1)) when (isnumericval nv1) -> nv1
  | TmPred (fi, t1) ->
      let t1' = eval1 t1 in TmPred (fi, t1')
  | TmIsZero (_, TmZero(_)) -> TmTrue(dummyinfo)
  | TmIsZero (_, TmSucc(_, nv1)) when (isnumericval nv1) -> TmFalse(dummyinfo)
  | TmIsZero (fi, t1) -> let t1' = eval1 t1 in TmIsZero(fi, t1')
  | _ -> raise NoRuleApplies

let eval t = 
  let rec aux t = 
    try let t' = eval1 t in aux t'
    with NoRuleApplies -> t
  in aux t
  


(* typeing *)

let rec typeof = function
  | TmTrue(_) -> TyBool
  | TmFalse(_) -> TyBool
  | TmIf(fi,t1,t2,t3) ->
      if (=) (typeof t1) TyBool then
        let (tyT2, tyT3) = (typeof t2, typeof t3) in
        if (=) tyT2 tyT3 then tyT2
        else error fi "arms of conditional have different types"
      else error fi "guard of conditional not a boolean"
  | TmZero(fi) -> TyNat
  | TmSucc(fi,t1) ->
      if (=) (typeof t1) TyNat then TyNat
      else error fi "argument of succ is not a number"
  | TmPred(fi, t1) ->
      if (=) (typeof t1) TyNat then TyNat
      else error fi "argument of pred is not a number"
  | TmIsZero(fi, t1) ->
      if (=) (typeof t1) TyNat then TyBool
      else error fi "argument of iszero is not a number"
