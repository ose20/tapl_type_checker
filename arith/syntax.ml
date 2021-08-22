open Format
open Support.Error
open Support.Pervasive

(* ---------------------------------------------------------------- *)
(* Datatypes *)

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

(* ---------------------------------------------------------------- *)

let info_of_tm = function
  | TmTrue fi | TmFalse fi | TmIf (fi, _, _, _)
  | TmZero fi | TmSucc (fi, _) | TmPred (fi, _)
  | TmIsZero (fi, _) -> fi

(* ---------------------------------------------------------------- *)
(* Printing *)

let obox () = open_hovbox 0
let cbox () = close_box ()
let cut () = print_cut ()
let break a b = print_break a b

let rec printtm_Term = function
  | TmIf (_, t1, t2, t3) ->
			open_vbox 0;
			pr "if";
			break 0 2;
		
			printtm_Term t1;		
			
			cut ();
			pr "then";
			break 0 2;

			printtm_Term t2;

			cut ();
			pr "else";
			break 0 2;

			printtm_Term t3;

			cbox ()
and printtm_AppTerm = function
  | TmPred (_, t1) ->
      obox (); pr "pred ";
      printtm_ATerm false t1; cbox ();
  | TmIsZero (_, t1) ->
      obox (); pr "iszero ";
      printtm_ATerm false t1; cbox ();
  | t -> printtm_ATerm false t
and printtm_ATerm not_val = function
  | TmTrue _ -> obox (); pr "true"; cbox ()
  | TmFalse _ -> obox (); pr "false"; cbox ()
  | TmZero _ -> obox (); pr "0"; cbox ()
  | TmSucc (_, t1) as term ->
      (*  succ (succ (... (0))) だったら整数リテラルにしたいけど， 
      **  そうでない場合は succ をプリントする．                   
      **  TmSucc の部分木に succ 以外のものが来ないかのチェックを　
      **  何回もやらないようにアルゴリズムを工夫する． *)
      if not_val then
        begin
            obox (); pr "(succ "; printtm_ATerm not_val t1;
            pr ")"; cbox ();
        end
      else
        let rec aux n = function
        | TmZero _ -> obox (); pr (string_of_int n); cbox ();
        | TmSucc (_, s) -> aux (n + 1) s
        | _ -> printtm_ATerm true term
        in aux 1 t1

let printtm t = printtm_Term t
