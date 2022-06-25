open Syntax
open Support.Error
open Support.Pervasive

(* -------------------- *)
(* evaluation *)

exception No_rule_applies

let rec is_numeric_val ctx = function
  | Tm_zero _ -> true
  | Tm_succ (_, t1) -> is_numeric_val ctx t1
  | _ -> false

let is_val ctx = function
  | Tm_true _ -> true
  | Tm_false _ -> true
  | t when is_numeric_val ctx t -> true
  | Tm_abs (_, _, _, _) -> true
  | _ -> false

let rec eval1 ctx = function
  | Tm_app (_, Tm_abs(_, _, _, t12), v2) when is_val ctx v2 ->
      b_reduce t12 v2
  | Tm_app (fi, v1, t2) when is_val ctx v1 ->
      let t2' = eval1 ctx t2 in
      Tm_app (fi, v1, t2')
  | Tm_app (fi, t1, t2) ->
      let t1' = eval1 ctx t1 in
      Tm_app (fi, t1', t2) 
  | Tm_if (_, Tm_true _, t2, _) -> t2
  | Tm_if (_, Tm_false _, _, t3) -> t3
  | Tm_if (fi, t1, t2, t3) ->
      let t1' = eval1 ctx t1 in
      Tm_if (fi, t1', t2, t3)
  | Tm_succ (fi, t1) ->
      let t1' = eval1 ctx t1 in
      Tm_succ (fi, t1')
  | Tm_pred (_, Tm_zero _) -> Tm_zero dummy_info
  | Tm_pred (_, Tm_succ (_, nv1)) when (is_numeric_val ctx nv1) -> nv1
  | Tm_pred (fi, t1) ->
      let t1' = eval1 ctx t1 in
      Tm_pred (fi, t1')
  | Tm_iszero (_, Tm_zero _) -> Tm_true dummy_info
  | Tm_iszero (_, Tm_succ (_, nv1)) when (is_numeric_val ctx nv1) ->
      Tm_false dummy_info
  | Tm_iszero (fi, t1) ->
      let t1' = eval1 ctx t1 in
      Tm_iszero (fi, t1')
  | Tm_let (_, _, v1, t2) when is_val ctx v1 ->
      b_reduce t2 v1
  | Tm_let (fi, x, t1, t2) ->
      let t1' = eval1 ctx t1 in
      Tm_let (fi, x, t1', t2)
  | _ ->
      raise No_rule_applies

let rec eval ctx t =
  try
    let t' = eval1 ctx t in
    eval ctx t'
  with No_rule_applies -> t

(* -------------------- *)
(* Typing *)

type constr = (ty * ty) list
let empty_constr = []
let combine_constr = List.append

let print_constr constr =
  let print_eq (ty_s, ty_t) =
    printty ty_s; pr "="; printty ty_t in
  let rec aux = function
    | [] -> ()
    | [eq] -> print_eq eq
    | eq::rest -> print_eq eq; pr ", "; aux rest
  in
  pr "{"; aux constr; pr "}"


(* next unique variable *)
type next_uvar = Next_uvar of string * uvar_generator
and uvar_generator = unit -> next_uvar

let uvar_gen =
  let rec f n () = Next_uvar ("?X" ^ string_of_int n, f (n+1))
  in f 0

let rec recon ctx next_uvar = function
  | Tm_var (fi, i, _) ->
      let ty_t = get_type fi ctx i in
      (ty_t, next_uvar, [])
  | Tm_abs (_, x, Some ty_t1, t2) ->
      let ctx' = add_binding ctx x (Var_bind ty_t1) in
      let (ty_t2, next_uvar2, constr2) = recon ctx' next_uvar t2 in
      (Ty_arr (ty_t1, ty_t2), next_uvar2, constr2)
  | Tm_abs (_, x, None, t2) ->
      let Next_uvar (u, next_uvar0) = next_uvar () in
      let ty_x = Ty_id u in
      let ctx' = add_binding ctx x (Var_bind ty_x) in
      let (ty_t2, next_uvar2, constr2) = recon ctx' next_uvar0 t2 in
      (Ty_arr (ty_x, ty_t2), next_uvar2, constr2)
  | Tm_app (_, t1, t2) ->
      let (ty_t1, next_uvar1, constr1) = recon ctx next_uvar t1 in
      let (ty_t2, next_uvar2, constr2) = recon ctx next_uvar1 t2 in
      let Next_uvar (new_var, next_uvar') = next_uvar2 () in
      let new_constr = (ty_t1, Ty_arr (ty_t2, Ty_id new_var)) :: (combine_constr constr1 constr2) in
      (Ty_id new_var, next_uvar', new_constr)
  | Tm_let (_, x, t1, t2) ->
      if not (is_val ctx t1) then
        let (ty_t1, next_uvar1, constr1) = recon ctx next_uvar t1 in
        let ctx1 = add_binding ctx x (Var_bind ty_t1) in
        let (ty_t2, next_uvar2, constr2) = recon ctx1 next_uvar1 t2 in
        (ty_t2, next_uvar2, constr1 @ constr2)
      else
        recon ctx next_uvar (b_reduce t2 t1)
  | Tm_zero _ -> (Ty_nat, next_uvar, [])
  | Tm_succ (_, t1) | Tm_pred (_, t1) | Tm_iszero (_, t1) ->
      let (ty_t1, next_uvar1, constr1) = recon ctx next_uvar t1 in
      (Ty_nat, next_uvar1, (ty_t1, Ty_nat) :: constr1)
  | Tm_true _ -> (Ty_bool, next_uvar, [])
  | Tm_false _ -> (Ty_bool, next_uvar, [])
  | Tm_if (_, t1, t2, t3) ->
      let (ty_t1, next_uvar1, constr1) = recon ctx next_uvar t1 in
      let (ty_t2, next_uvar2, constr2) = recon ctx next_uvar1 t2 in
      let (ty_t3, next_uvar3, constr3) = recon ctx next_uvar2 t3 in
      let new_constr = [(ty_t1, Ty_bool); (ty_t2, ty_t3)] in
      (ty_t2, next_uvar3, List.concat [new_constr; constr1; constr2; constr3])

let type_subst ty_x ty_t ty_s = (* [x -> t]s *)
  let rec aux = function
    | Ty_arr (ty_s1, ty_s2) -> Ty_arr (aux ty_s1, aux ty_s2)
    | Ty_nat -> Ty_nat
    | Ty_bool -> Ty_bool
    | (Ty_id _) as it -> if ty_x = it then ty_t else it
  in aux ty_s

let apply_subst subst ty_t =
  List.fold_left 
    (fun ty_t (ty_x, ty_s) -> match ty_x with
      | Ty_id _ ->
          type_subst ty_x ty_s ty_t
      | _ ->
          errs "ill-formed type substitution")
    ty_t (List.rev subst)

let type_subst_in_constr ty_x ty_t constr =
  List.map
    (fun (ty_s1, ty_s2) ->
      (type_subst ty_x ty_t ty_s1, type_subst ty_x ty_t ty_s2))
    constr

let occurs_in ty_x ty_t =
  let rec aux = function
    | Ty_arr (ty_t1, ty_t2) -> aux ty_t1 || aux ty_t2
    | Ty_nat -> false
    | Ty_bool -> false
    | (Ty_id _) as it -> ty_x = it
  in aux ty_t

let unify fi msg constr =
  let rec u = function
    | [] -> []
    | (ty_s, Ty_id x) :: rest ->
        if ty_s = Ty_id x then u rest
        else if occurs_in (Ty_id x) ty_s then
          errs_at fi (msg ^ ": circular constraints")
        else
          List.append (u (type_subst_in_constr (Ty_id x) ty_s rest)) 
                      [(Ty_id x, ty_s)]
    | (Ty_id x, ty_t) :: rest ->
        if Ty_id x = ty_t then u rest
        else if occurs_in (Ty_id x) ty_t then
          errs_at fi (msg ^ ": circular constraints")
        else
          List.append (u (type_subst_in_constr (Ty_id x) ty_t rest))
                      [(Ty_id x, ty_t)]
    | (Ty_nat, Ty_nat) :: rest -> u rest
    | (Ty_bool, Ty_bool) :: rest -> u rest
    | (Ty_arr (ty_s1, ty_s2), Ty_arr (ty_t1, ty_t2)) :: rest ->
        u ((ty_s1, ty_t1) :: (ty_s2, ty_t2) :: rest)
    | _ ->
        errs_at fi "Unsolvable constraints"
  in
  u constr