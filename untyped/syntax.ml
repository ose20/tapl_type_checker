open Format
open Support.Error
open Support.Pervasive

(* ------------------------------------------------------------------- *)
(* DateType *)

type term =
	(* The third attribute "int" is the length of the environment in which this TmVar is located *)
	| TmVar of info * int * int
	| TmAbs of info * string * term
	| TmApp of info * term * term

type context = string list

type command =
	| Import of string
	| Eval of info * term
	| Bind of info * string

(* ------------------------------------------------------------------- *)
(* Context managemant *)

let emptycontext : (string list) = []
let ctxlen (ctx : string list) = List.length ctx
let addname ctx (x : string) = x :: ctx
let pickfreshname ctx x =
	let rec aux x idx =
		let x' = x ^ (if idx = 0 then "" else string_of_int idx) in
		if List.mem x' ctx
		then aux x (idx+1)
		else (addname ctx x'), x'
	in aux x 0

let rec name2index fi ctx x =
	match ctx with
	| [] -> errsAt fi ("Identifier " ^ x ^ " is unbound")
	| y :: rest ->
			if x=y then 0 else 1 + (name2index fi rest x)

let index2name fi ctx n =
	try List.nth ctx n
	with Failure _ ->
		let msg = Printf.sprintf "Variable lookup failure: offset: %d, ctx size %d" n (ctxlen ctx) in
		errsAt fi msg

(* ------------------------------------------------------------------- *)
(* Shifting *)

let shiftTermAbove c d t =
	let rec walk c = function
	| TmVar (fi, x, n) ->
			if x >= c then TmVar (fi, x+d, n+d) else TmVar (fi, x, n+d)
	| TmAbs (fi, str, t1) ->
			TmAbs (fi, str, walk (c+1) t1)
	| TmApp (fi, t1, t2) ->
			TmApp (fi, walk c t1, walk c t2)
	in walk c t

let shiftTerm d t = shiftTermAbove 0 d t

(* ------------------------------------------------------------------- *)
(* Substitution*)

let substTerm j s t =
	let rec walk c = function
	| TmVar (fi, x, n) as t ->
			if j+c = x then shiftTerm c s else t
	| TmAbs (fi, str, t1) ->
			TmAbs (fi, str, walk (c+1) t1)
	| TmApp (fi, t1, t2) ->
			TmApp (fi, walk c t1, walk c t2)
	in walk 0 t

(* beta-reduction of the term (\lambda. t)v *)			
let betaReduction t v = 
	shiftTerm (-1) (substTerm 0 (shiftTerm 1 v) t)

(* ------------------------------------------------------------------- *)
(* Extracting file info *)

let info_of_term = function
	| TmVar (fi, _, _) -> fi
	| TmAbs (fi, _, _) -> fi
	| TmApp (fi, _, _) -> fi


(* ------------------------------------------------------------------- *)
(* Printing *)

let rec printtm_Term ctx = function
	| TmAbs (_, x, t1) ->
			let ctx', x' = pickfreshname ctx x in
			hovbox 2;
			printf "lambda %s." x';
			space ();
			printtm_Term ctx' t1;
			cbox ()
	| t -> printtm_AppTerm ctx t
and printtm_AppTerm ctx = function
	| TmApp (_, t1, t2) ->
			hovbox 0;
			printtm_AppTerm ctx t1;
			space ();
			printtm_ATerm ctx t2;
			cbox ()
	| t -> printtm_ATerm ctx t	 
and printtm_ATerm ctx = function
	| TmVar (fi, x, n) ->
			if ctxlen ctx = n then
				(hovbox 0;
				pr (index2name fi ctx x);
				cbox ())
			else
				printf "@[<hov 0>[bad index: %s/%s in {%s}]@]"
				(string_of_int x) (string_of_int n)
				(List.fold_left (fun lst elt -> if lst = "" then elt else lst ^ "; " ^ elt)
				"" ctx)
	| t ->
			hovbox 0;
			pr "(";
			printtm_Term ctx t;
			pr ")";
			cbox ()
let printtm ctx t = 
	printtm_Term ctx t;
	pr ";"; newline ()