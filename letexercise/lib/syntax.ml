open Format
open Support.Error
open Support.Pervasive

(* ------------------------- Datatypes ------------------------------*)
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

(* Date type to represent the type of the variable *)
type binding = 
  | Name_bind
  | Var_bind of ty

(* list consisting of pairs of variable names and their types *)
type context = (string * binding) list

type command =
  | Eval  of info * term

(* ------------------------------------------------------------------ *)

let info_of_term = function
  | Tm_var    (i, _, _) -> i
  | Tm_abs    (i, _, _, _) -> i
  | Tm_app    (i, _, _) -> i
  | Tm_letin  (i, _, _, _) -> i
  | Tm_true   i -> i
  | Tm_false  i -> i
  | Tm_if     (i, _, _, _) -> i


(* --------------------- Context management ---------------------------- *)

let (empty_context : (string * binding) list) = []

let ctx_len = List.length
let add_binding ctx x bind = (x, bind) :: ctx
let add_name ctx x = add_binding ctx x Name_bind

let index2name = fun fi ctx n ->
  try
    let (xn, _) = List.nth ctx n in xn
  with
    Failure _ ->
      let msg =
        Printf.sprintf "Variable lookup failure: offset: %d, ctx size: %d"
      in errs_at fi @@ msg n @@ ctx_len ctx

let rec name2index = fun fi ctx x ->
  match ctx with
    | [] -> errs_at fi ("Identifier " ^ x ^ " is unbound")
    | (y, _)::rest ->
        if x=y then 0
        else 1 + name2index fi rest x

let get_binding fi ctx i =
  try let (_, bind) = List.nth ctx i in bind
  with Failure _ ->
    let msg = Printf.sprintf "Variable lookup failure: offset: %d, ctx size: %d"
    in errs_at fi (msg i @@ ctx_len ctx)

let get_type_from_ctx fi ctx i =
  match get_binding fi ctx i with
    | Var_bind ty -> ty
    | _ -> errs_at fi @@
            "get_type_from_context: Wrong kind of binding for variable" ^ index2name fi ctx i

(* -------------------------------------------------------------------- *)

(* ------------------------ Nameless term management ------------------ *)
(* Abstract two functions: shift_with_cut and subst *)
let tmmap onvar c t = 
  let rec walk c = function
    | Tm_var (fi, x, n) -> 
        onvar fi x n c
    | Tm_abs (fi, x, ty, t) -> Tm_abs (fi, x, ty, walk (c+1) t)
    | Tm_app (fi, t1, t2) -> Tm_app (fi, walk c t1, walk c t2)
    | Tm_letin (fi, x, t1, t2) -> Tm_letin (fi, x, walk c t1, walk (c+1) t2)
    | Tm_true (_) as t -> t
    | Tm_false (_) as t -> t
    | Tm_if (fi, t1, t2, t3) -> Tm_if (fi, walk c t1, walk c t2, walk c t3)
  in walk c t

let shift_with_cut d c t =
  tmmap
    (fun fi x n c -> if x>=c then Tm_var (fi, x+d, n+d) else Tm_var (fi, x, n))
  c t

(*
let shift_with_cut d c t =
  let rec walk c = function
    | Tm_var (fi, x, n) as var ->
        if x>=c then Tm_var (fi, x+d, n+d) else var (* fi, d *)
    | Tm_abs (fi, x, ty, t) -> Tm_abs (fi, x, ty, walk (c+1) t)
    | Tm_app (fi, t1, t2) -> Tm_app (fi, walk c t1, walk c t2)
    | Tm_letin (fi, x, t1, t2) ->Tm_letin (fi, x, walk c t1, walk (c+1) t2)
    | Tm_true (_) as t -> t
    | Tm_false (_) as t -> t
    | Tm_if (fi, t1, t2, t3) -> Tm_if (fi, walk c t1, walk c t2, walk c t3)
  in walk c t
*)

let shift d t = shift_with_cut d 0 t

let subst j s t = 
  tmmap
    (fun fi x n c -> if x=j+c then shift c s else Tm_var (fi, x, n))
  0 t

(*
let subst j s t = 
  let rec walk c = function
    | Tm_var (_, x, _) as var -> 
        if x=j+c then shift c s else var (*  *)
    | Tm_abs (fi, x, ty, t) -> Tm_abs (fi, x, ty, walk (c+1) t)
    | Tm_app (fi, t1, t2) -> Tm_app (fi, walk c t1, walk c t2)
    | Tm_letin (fi, x, t1, t2) -> Tm_letin (fi, x, walk c t1, walk (c+1) t2)
    | Tm_true (_) as t -> t
    | Tm_false (_) as t -> t
    | Tm_if (fi, t1, t2, t3) -> Tm_if (fi, walk c t1, walk c t2, walk c t3)
  in walk 0 t
*)

let subst_top t v = (* reduction of (\.t)v *)
  shift (-1) @@ subst 0 (shift 1 v) t
(* ------------------------------------------------------------------*)

(* ------------------------- Evaluation ---------------------------- *)
exception No_rule_applies
let isval = function
  | Tm_true (_) | Tm_false (_) | Tm_abs (_, _, _, _) -> true
  | _ -> false

let rec eval1 = function
  | Tm_app (_, Tm_abs(_, _ , _, t12), v2) when isval v2 -> subst_top t12 v2
  | Tm_app (fi, v1, t2) when isval v1 -> 
      let t2' = eval1 t2 in Tm_app (fi, v1, t2')
  | Tm_app (fi, t1, t2) ->
      let t1' = eval1 t1 in Tm_app (fi, t1', t2)
  | Tm_if (_, Tm_true _, t2, _) -> t2
  | Tm_if (_, Tm_false _, _, t3) -> t3
  | Tm_if (fi, t1, t2, t3) -> 
      let t1' = eval1 t1 in Tm_if (fi, t1', t2, t3)
  | Tm_letin (_, _, v1, t2) when isval v1 -> subst_top t2 v1
  | Tm_letin (fi, x, t1, t2) ->
      let t1' = eval1 t1 in Tm_letin (fi, x, t1', t2)
  | _ -> raise No_rule_applies

let eval t =
  let rec aux t =
    try let t' = eval1 t in aux t'
    with No_rule_applies -> t
  in aux t
(* ------------------------------------------------------------------ *)


(* -------------------------- Typing  ------------------------------- *)
let rec type_of ctx = function
  | Tm_var (fi, i, _) -> get_type_from_ctx fi ctx i
  | Tm_abs (_, x, ty1, t2) ->
      let ctx' = add_binding ctx x (Var_bind ty1) in
      let ty2 = type_of ctx' t2 in
      Ty_arr (ty1, ty2)
  | Tm_app (fi, t1, t2) ->
      let (ty1, ty2) = (type_of ctx t1, type_of ctx t2) in
      begin
        match ty1 with
          | Ty_arr (ty11, ty12) ->
              if (=) ty11 ty2 then ty12
              else errs_at fi "parameter type mismatch"
          | _ -> errs_at fi "arrow type expected"
      end
  | Tm_letin (_, x, t1, t2) ->
      let ty1 = type_of ctx t1 in
      let ctx' = add_binding ctx x (Var_bind ty1) in
      type_of ctx' t2
  | Tm_true _ | Tm_false _ -> Ty_bool
  | Tm_if (fi, t1, t2, t3) ->
      if (=) (type_of ctx t1) Ty_bool then
        let (ty2, ty3) = (type_of ctx t2, type_of ctx t3) in
        if (=) ty2 ty3 then ty2
        else errs_at fi "arms of conditional have different types"
      else errs_at fi "guard of conditional not a boolean"
(* ----------------------------------------------------------------- *)


(* ---------------------------- Printing --------------------------- *)

let rec printty_arrow_type = function
  | Ty_arr (ty1, ty2) ->
      open_hovbox 0;
      printty_a_type ty1;
      space();
      pr "-> ";
      printty_arrow_type ty2;
      close_box()
  | ty -> printty_a_type ty
and printty_a_type = function
  | Ty_bool -> pr "Bool"
  | ty -> pr "("; printty_arrow_type ty; pr ")"

let printty = printty_arrow_type


let rec printtm_term ctx = function
  | Tm_abs (_, x, ty1, t2) ->
    begin
      let ctx' = add_name ctx x in
      open_hvbox 2; pr "\\";
      pr x; pr ":"; printty ty1; pr "."; space();
      printtm_term ctx' t2
    end
  | Tm_letin (_, x, t1, t2) ->
      open_hvbox 0;
      pr "let "; pr x; pr " ="; 
      open_hvbox 2;
      space();
      printtm_term ctx t1;
      close_box();
      break 1 0; pr "in";
      space();
      printtm_term ctx t2;
      close_box()
  | Tm_if (_, t1, t2, t3) ->
      open_hvbox 2;
      pr "if"; break 1 2;
      printtm_term ctx t1; break 1 0;
      pr "then"; break 1 2;
      printtm_term ctx t2; break 1 0;
      pr "else"; break 1 2;
      printtm_term ctx t3;
      close_box()
  | t -> printtm_app_term ctx t
and printtm_app_term ctx = function
  | Tm_app (_, t1, t2) ->
      open_hvbox 0;
      printtm_app_term ctx t1; space();
      printtm_a_term ctx t2;
      close_box()
  | t -> printtm_a_term ctx t
and printtm_a_term ctx = function
  | Tm_var (fi, x, n) ->
      if ctx_len ctx = n then
        pr (index2name fi ctx x)
      else
        pr @@
        "[bad index: " ^ string_of_int x ^ "/" ^ string_of_int n
        ^ " in {" ^ List.fold_left (fun s (x,_) -> s ^ " " ^ x) "" ctx
        ^ " }]"
  | Tm_true _ -> pr "true"
  | Tm_false _ -> pr "false"
  | t -> pr "("; printtm_term ctx t; pr ")"

let printtm = printtm_term

let print_tmty ctx t =
  open_hvbox 2;
  pr "-";
  pr " : ";
  printty @@ type_of ctx t;
  space();
  pr "=";
  space();
  printtm ctx t;
  print_newline();
  close_box();

