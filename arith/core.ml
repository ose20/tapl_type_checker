open Syntax
open Support.Error

(* -------------------------- EVALUATION ------------------------------------- *)

exception NoRuleApplies

let rec isnumericval = function
  | TmZero _ -> true
  | TmSucc (_, t1) -> isnumericval t1
  | _ -> false

let isval = function
  | TmTrue _ -> true
  | TmFalse _ -> true
  | t when isnumericval t -> true
  | _ -> false

(* 1-step evaluation *)    
let rec eval1 = function
  | TmIf (_, TmTrue _, t2, t3) -> t2
  | TmIf (_, TmFalse _, t2, t3) -> t3
  | TmIf (fi, t1, t2, t3) ->
			let t1' = eval1 t1 in TmIf (fi, t1', t2, t3)
  | TmSucc (fi, t1) ->
      let t1' = eval1 t1 in TmSucc (fi, t1')
  | TmPred (fi, TmZero _) -> TmZero (fi)
  | TmPred (fi, TmSucc (_, t1)) when isnumericval t1 -> t1
  | TmPred (fi, t1) ->
      let t1' = eval1 t1 in TmPred (fi, t1')
  | TmIsZero (fi, TmZero _) -> TmTrue fi
  | TmIsZero (fi, TmSucc (_, t1)) when isnumericval t1 -> TmFalse fi
  | TmIsZero (fi, t1) ->
      let t1' = eval1 t1 in TmIsZero (fi, t1')
  | _ -> raise NoRuleApplies

(* 評価できなくなるまで評価する *)    
let rec eval t =
  try let t' = eval1 t in eval t' with
  | NoRuleApplies -> t