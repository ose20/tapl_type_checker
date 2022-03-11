open Format
open Support.Error
open Support.Pervasive

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


(* Extracting file info *)
let tmInfo t = match t with
  | TmTrue(fi) -> fi
  | TmFalse(fi) -> fi
  | TmIf(fi,_,_,_) -> fi
  | TmZero(fi) -> fi
  | TmSucc(fi,_) -> fi
  | TmPred(fi,_) -> fi
  | TmIsZero(fi,_) -> fi


(* ---------------------------------------- *)
(* Printing *)

let obox () = open_hovbox 0
let cbox () = close_box ()
let cut () = print_cut ()
let break a b = print_break a b

let rec printty_Type outer tyT = match tyT with
  tyT -> printty_AType outer tyT
and printty_AType outer tyT = match tyT with
  | TyBool -> pr "Bool"
  | TyNat -> pr "Nat"

let printty tyT = 
  obox ();
  printty_Type true tyT;
  cbox()

let rec printtm_Term outer t = match t with
  | TmIf(fi, t1, t2, t3) ->
      open_vbox 0;
      pr "if";
      break 0 2;

      printtm_Term false t1;

      cut ();
      pr "then";
      break 0 2;

      printtm_Term false t2;

      cut ();
      pr "else";
      break 0 2;

      printtm_Term false t3;

      cbox ()
  | t -> printtm_AppTerm outer t
and printtm_AppTerm outer t = match t with
  | TmPred(_, t1) ->
      obox (); pr "pred "; printtm_ATerm false false t1; cbox ();
  | TmIsZero(_, t1) ->
      obox (); pr "iszero "; printtm_ATerm false false t1; cbox ()
  | t -> printtm_ATerm false outer t
and printtm_ATerm notLiteral outer t = match t with
  | TmTrue _ -> obox (); pr "true"; cbox ();
  | TmFalse _ -> obox (); pr "false"; cbox ();
  | TmZero _ -> obox (); pr "0"; cbox ();
  | TmSucc(_, t1) as term ->
      (*  succ (succ (... (succ 0))) の場合のみ整数リテラルに変換したい．
      **  部分項が条件を満たさないようにチェックするが，同じ計算を何度もしないように
      **  アルゴリズムを工夫する． *)
      if notLiteral then
        begin
          obox ();
          pr "(succ "; printtm_ATerm notLiteral outer t1; pr ")";
          cbox ();
        end
      else
        let rec aux n = function
          | TmZero _ -> obox (); pr (string_of_int n); cbox ();
          | TmSucc (_, s) -> aux (n + 1) s
          | _ -> printtm_ATerm true outer term
        in aux 1 t1
  | t ->
      obox ();
      pr "("; printtm_Term outer t; pr ")"; cbox ()

let printtm t = 
  printtm_Term true t;

