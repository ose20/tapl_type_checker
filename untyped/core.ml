(* open Format *)
open Syntax
(* open Support.Error *)
(* open Support.Pervasive *)

(* ---------------------------------------------------------------------------------- *)
(* Evaluation *)

let isval = function
  | TmAbs (_, _, _) -> true
  | _ -> false

exception NoRuleApplies

let rec eval1 = function
  | TmApp(fi, TmAbs(_, x, t12), v2) when isval v2 ->
      betaReduction t12 v2
  | TmApp(fi, v1, t2) when isval v1 ->
      let t2' = eval1 t2 in
      TmApp(fi, v1, t2')
  | TmApp(fi, t1, t2) ->
      let t1' = eval1 t1 in
      TmApp(fi, t1', t2)
  | _ ->
      raise NoRuleApplies

let rec eval t =
  try
    let t' = eval1 t in eval t'
  with NoRuleApplies -> t