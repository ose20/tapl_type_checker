open Format
open Support.Error
open Support.Pervasive

(* Datatypes *)
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

(* 変数の型を追跡する *)
type binding =
  | NameBind
  | VarBind of ty


(* 変数の名前とその型の組みのリスト *)
type context = (string * binding) list

type command = 
  | Import of string
  | Eval of info * term
  | Bind of info * string * binding


(* Context management *)
let (empty_context : (string * binding) list) = []

let ctx_len = List.length
let add_binding ctx x bind = (x,bind) :: ctx
let add_name ctx x = add_binding ctx x NameBind

let rec is_name_bound ctx x =
  match ctx with
    | [] -> false
    | (y,_)::rest ->
        if y=x then true
        else is_name_bound rest x

let rec pick_fresh_name ctx x =
  if is_name_bound ctx x then pick_fresh_name ctx (x^"'")
  else ((x,NameBind)::ctx, x)


(* context の x 番目の変数（文字列）を取り出す *)
let index2name = fun fi ctx x ->
  try
    let (xn, _) = List.nth ctx x in xn
  with Failure _ ->
    let msg = 
      Printf.sprintf "Variable lookup failure: offset: %d, ctx size: %d"
    in errs_at fi (msg x (ctx_len ctx))


(* context において 変数 x を 名無し表現にする *)
let rec name2index = fun fi ctx x ->
  match ctx with
    | [] -> errs_at fi ("Identifier " ^ x ^ " is unbound")
    | (y, _)::rest ->
        if y=x then 0
        else 1 + (name2index fi rest x)


(* shifting *)
let tmmap onvar c t =
  let rec walk c t = match t with
    | TmVar (fi,x,n) -> onvar fi c x n
    | TmAbs (fi,x,tyT1,t2) -> TmAbs (fi,x,tyT1,walk (c+1) t2)
    | TmApp (fi,t1,t2) -> TmApp(fi,walk c t1, walk c t2)
    | TmTrue (_) as t -> t
    | TmFalse (_) as t -> t
    | TmIf(fi,t1,t2,t3) -> TmIf (fi, walk c t1, walk c t2, walk c t3)
  in walk c t

let term_shift_above d c t =
  tmmap
    (fun fi c x n -> if x>=c then TmVar (fi,x+d,n+d) else TmVar(fi,x,n+d))
    c t

let term_shift d t = term_shift_above d 0 t


(* Substitution *)
let term_subst j s t =
  tmmap
    (fun fi j x n -> if x=j then term_shift j s else TmVar(fi,x,n))
  j t

(* (\.t)s の簡約 *)
let term_subst_top s t =
  term_shift (-1) (term_subst 0 (term_shift 1 s) t) 


(* Context management *)
let get_binding fi ctx i =
  try
    let (_,bind) = List.nth ctx i in bind
  with Failure _ ->
    let msg = 
      Printf.sprintf "Variable lookup failure: offset: %d, ctx size: %d"
    in errs_at fi (msg i (ctx_len ctx))

let get_type_from_context fi ctx i =
  match get_binding fi ctx i with
    | VarBind tyT -> tyT
    | _ -> errs_at fi
            @@ "getTypeFromContext: Wrong kind of binding for variable"
            ^ index2name fi ctx i

(* Extracting file info *)
let info_of_term = function
  | TmVar (fi,_,_) -> fi
  | TmAbs (fi,_,_,_) -> fi
  | TmApp (fi,_,_) -> fi
  | TmTrue fi -> fi
  | TmFalse fi -> fi
  | TmIf (fi,_,_,_) -> fi

(* typing *)
let rec typeof ctx = function
  | TmVar(fi,i,_) -> get_type_from_context fi ctx i
  | TmAbs(_,x,tyT1,t2) ->
      let ctx' = add_binding ctx x (VarBind(tyT1)) in
      let tyT2 = typeof ctx' t2 in
      TyArr(tyT1, tyT2)
  | TmApp(fi,t1,t2) ->
      let tyT1 = typeof ctx t1 in
      let tyT2 = typeof ctx t2 in
      (match tyT1 with
        | TyArr(tyT11,tyT12) ->
          if (=) tyT2 tyT11 then tyT12
          else errs_at fi "parameter type mismatch"
        | _ -> errs_at fi "arrow type expected")
  | TmTrue(_) -> TyBool
  | TmFalse(_) -> TyBool
  | TmIf(fi,t1,t2,t3) ->
      if (=) (typeof ctx t1) TyBool then
        let tyT2 = typeof ctx t2 in
        if (=) tyT2 (typeof ctx t3) then tyT2
        else errs_at fi "arms of conditional have different types"
      else errs_at fi "guard of conditional not a boolean"


(* Printing *)
let small = function
  | TmVar _ -> true
  | _ -> false

let rec printty_type outer tyT = printty_arrow_type outer tyT
and printty_arrow_type outer = function
  | TyArr (ty1, ty2) ->
      open_hovbox 0;
      printty_a_type false ty1;
      if outer then pr " ";
      pr "->";
      if outer then space() else break 0 0;
      printty_arrow_type outer ty2;
      close_box ()
  | tyT -> printty_a_type outer tyT
and printty_a_type outer = function
  | TyBool -> pr "Bool"
  | tyT -> pr "("; printty_type outer tyT; pr ")"

let printty ty = printty_type true ty

let rec printtm_term outer ctx = function
  | TmAbs(_,x,tyT1,t2) ->
    begin
      let (ctx', x') = (pick_fresh_name ctx x) in
      open_hvbox 2; pr "\\";
      pr x'; pr ":"; printty_type false tyT1; pr ".";
      if (small t2) && not outer then break 0 0 else space();
      printtm_term outer ctx' t2;
      close_box()
    end
  | TmIf(_,t1,t2,t3) ->
      open_hvbox 2;
      pr "if"; space();
      printtm_term false ctx t1; space();
      pr "then"; space();
      printtm_term false ctx t2; space();
      pr "else"; space();
      printtm_term false ctx t3;
      close_box()
  | t -> printtm_app_term outer ctx t
and printtm_app_term outer ctx = function
  | TmApp(_,t1,t2) ->
      open_hvbox 0;
      printtm_app_term false ctx t1; space();
      printtm_a_term false ctx t2;
      close_box()
  | t -> printtm_a_term outer ctx t      
and printtm_a_term outer ctx = function
  | TmVar(fi,x,n) ->
      if ctx_len ctx = n then
        pr (index2name fi ctx x)
      else
        pr @@
        "[bad index: " ^ string_of_int x ^ "/" ^ string_of_int n
        ^ " in {" ^ List.fold_left (fun s (x,_) -> s ^ " " ^ x) "" ctx
        ^ " }]"
  | TmTrue(_) -> pr "true"
  | TmFalse(_) -> pr "false"
  | t -> pr "("; printtm_term outer ctx t; pr ")"

let printtm ctx t = printtm_term true ctx t

(* print term and its type *)
let print_tmty ctx t =
  open_hvbox 0;
  printtm ctx t;
  break 1 2;
  pr ": ";
  printty @@ typeof ctx t;
  print_newline();
  close_box()


let prbinding = function
  | NameBind -> ()
  | VarBind (tyT) -> pr ": "; printty tyT


(* evaluation *)
let isval = function
  | TmTrue(_) -> true
  | TmFalse(_) -> true
  | TmAbs(_,_,_,_) -> true
  | _ -> false

exception NoRuleApplies

let rec eval1 = function
  | TmApp(_,TmAbs(_,_,_,t12),v2) when isval v2 ->
      term_subst_top v2 t12
  | TmApp(fi,v1,t2) when isval v1 ->
      let t2' = eval1 t2 in
      TmApp(fi,v1,t2')
  | TmApp(fi,t1,t2) ->
      let t1' = eval1 t1 in
      TmApp(fi,t1',t2)
  | TmIf(_,TmTrue(_),t2,_) -> t2
  | TmIf(_,TmFalse(_),_,t3) -> t3
  | TmIf(fi,t1,t2,t3) ->
      let t1' = eval1 t1 in
      TmIf(fi,t1',t2,t3)
  | _ ->
      raise NoRuleApplies

let rec eval t =
  try let t' = eval1 t in eval t'
  with NoRuleApplies -> t



