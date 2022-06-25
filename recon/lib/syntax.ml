open Format
open Support.Error
open Support.Pervasive

(* --------------- Data types --------------- *)

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


(* -------------------- *)
(* Context management *)


let (empty_context : (string * binding) list) = []
let ctx_len = List.length
let add_binding ctx x bind = (x, bind) :: ctx
let add_name ctx x = add_binding ctx x Name_bind

let index2name fi ctx idx =
  try 
    let (xn, _) = List.nth ctx idx in xn
  with
    Failure _ ->
      let msg =
        Printf.sprintf "Variable lookup failure: offset: %d, ctx size: %d"
      in errs_at fi @@ msg idx @@ ctx_len ctx

let rec name2index fi ctx x =
  match ctx with
  | [] -> errs_at fi ("Identifier " ^ x ^ " is unbound")
  | (y, _) :: rest ->
      if y = x then 0
      else 1 + (name2index fi rest x)

let get_binding fi ctx i =
  try
    snd @@ List.nth ctx i
  with
    Failure _ ->
      let msg = 
        Printf.sprintf "Variable lookup failure: offset: %d, ctx size: %d" in
      errs_at fi (msg i (ctx_len ctx))

let get_type fi ctx i =
  match get_binding fi ctx i with
  | Var_bind ty -> ty
  | _ -> errs_at fi @@
          "get_type: Wrong kind of binding for variable " ^ index2name fi ctx i

(* -------------------- *)
(* Shifting *)

let rec shift_above d c t = match t with
  | Tm_var (fi, n, len) as var ->
      if n < c then var else Tm_var (fi, n + d, len + d)
  | Tm_abs (fi, x, ty_t1, t2) -> 
      Tm_abs (fi, x, ty_t1, shift_above d (c+1) t2)
  | Tm_app (fi, t1, t2) ->
      Tm_app (fi, shift_above d c t1, shift_above d c t2)
  | Tm_true fi -> Tm_true fi
  | Tm_false fi -> Tm_false fi
  | Tm_if (fi, t1, t2, t3) -> 
      Tm_if (fi, shift_above d c t1, shift_above d c t2, shift_above d c t3)
  | Tm_zero fi -> Tm_zero fi
  | Tm_succ (fi, t1) -> Tm_succ (fi, shift_above d c t1)
  | Tm_pred (fi, t1) -> Tm_pred (fi, shift_above d c t1)
  | Tm_iszero (fi, t1) -> Tm_iszero (fi, shift_above d c t1)
  | Tm_let (fi, x, t1, t2) -> Tm_let (fi, x, shift_above d c t1, shift_above d (c+1) t2)

let shift d t = shift_above d 0 t

let rec subst j s t = match t with
  | Tm_var (_, n, _) as var ->
      if n = j then s else var
  | Tm_abs (fi, x, ty_t1, t2) -> 
      Tm_abs (fi, x, ty_t1, subst (j+1) (shift 1 s) t2)
  | Tm_app (fi, t1, t2) ->
      Tm_app (fi, subst j s t1, subst j s t2)
  | Tm_true fi -> Tm_true fi
  | Tm_false fi -> Tm_false fi
  | Tm_if (fi, t1, t2, t3) -> 
      Tm_if (fi, subst j s t1, subst j s t2, subst j s t3)
  | Tm_zero fi -> Tm_zero fi
  | Tm_succ (fi, t1) -> Tm_succ (fi, subst j s t1)
  | Tm_pred (fi, t1) -> Tm_pred (fi, subst j s t1)
  | Tm_iszero (fi, t1) -> Tm_iszero (fi, subst j s t1)
  | Tm_let (fi, x, t1, t2) -> 
      Tm_let (fi, x, subst j s t1, subst (j+1) (shift 1 s) t2)

let b_reduce t1 v2 = (* (\.t1)v2 --> ??? *)
  shift (-1) @@ subst 0 (shift 1 v2) t1


(* -------------------- *)
(* Extracting file info *)

let info_of_term = function
  | Tm_var (fi, _, _) -> fi
  | Tm_abs (fi, _, _, _) -> fi
  | Tm_app (fi, _, _) -> fi
  | Tm_true fi -> fi
  | Tm_false fi -> fi
  | Tm_if (fi, _, _, _) -> fi
  | Tm_zero fi -> fi
  | Tm_succ (fi, _) -> fi
  | Tm_pred (fi, _) -> fi
  | Tm_iszero (fi, _) -> fi
  | Tm_let (fi, _, _, _) -> fi

(* ------------------- *)
(* Printing *)

let rec printty_arrowtype = function
  | Ty_arr (ty1, ty2) ->
      open_hovbox 0;
      printty_atype ty1;
      space ();
      pr "-> ";
      printty_arrowtype ty2;
      close_box ()
  | ty -> printty_atype ty
and printty_atype = function
  | Ty_id b -> pr b
  | Ty_bool -> pr "Bool"
  | Ty_nat -> pr "Nat"
  | ty -> pr "("; printty_arrowtype ty; pr ")"

let printty = printty_arrowtype

let rec printtm_term ctx = function
  | Tm_abs (_, x, Some ty1, t2) ->
      let ctx' = add_name ctx x in
      open_hvbox 2; pr "\\";
      pr x; pr ":"; printty ty1; pr ".";
      printtm_term  ctx' t2;
      close_box ()
  | Tm_abs (_, x, None, t2) ->
      let ctx' = add_name ctx x in
      open_hvbox 2; pr "\\";
      pr x; pr ".";
      printtm_term ctx' t2;
  | Tm_if (_, t1, t2, t3) ->
      open_hvbox 0;
      pr "if"; break 1 2;
      printtm_term ctx t1; break 1 0;
      pr "then"; break 1 2;
      printtm_term ctx t2; break 1 0;
      pr "else"; break 1 2;
      printtm_term ctx t3;
      close_box () 
  | Tm_let (_, x, t1, t2) ->
      open_hvbox 2;
      pr "let "; pr x; pr " =";
      space ();
      printtm_term ctx t1;
      space ();
      pr "in";
      space ();
      printtm_term ctx t2;
      close_box ()
  | t -> printtm_appterm ctx t
and printtm_appterm ctx = function
  | Tm_app (_, t1, t2) ->
      open_hvbox 0;
      printtm_appterm ctx t1;
      space ();
      printtm_aterm ctx t2;
      close_box ()
  | Tm_pred (_, t1) ->
      pr "pred "; printtm_aterm ctx t1
  | Tm_iszero (_, t1) ->
      pr "iszero "; printtm_aterm ctx t1
  | t -> printtm_aterm ctx t
and printtm_aterm ctx = function
  | Tm_var (fi, n, len) ->
      if ctx_len ctx = len then
        pr (index2name fi ctx n)
      else
        pr @@ "[bad index: " ^ string_of_int n ^ "/" ^ string_of_int len
        ^ " in { "
        ^ (List.fold_left (fun s (x,_) -> s ^ " " ^ x) "" ctx)
        ^ " }]"
  | Tm_true _ -> pr "true"
  | Tm_false _ -> pr "false"
  | Tm_zero _ -> pr "0"
  | Tm_succ (_, t1) ->
      let rec f n = function
      | Tm_zero _ -> pr @@ string_of_int n
      | Tm_succ (_, t2) -> f (n+1) t2
      | _ -> pr "(succ "; printtm_aterm ctx t1; pr ")" (* bug? succ succ succ pred succ 0 とかで試してみたい *)
      in f 1 t1
  | t -> pr "("; printtm_term ctx t; pr ")"

let printtm = printtm_term
