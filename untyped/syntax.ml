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

(* ------------------------------------------------------------------- *)
(* Extracting file info *)

let info_of_term = function
	| TmVar (fi, _, _) -> fi
	| TmAbs (fi, _, _) -> fi
	| TmApp (fi, _, _) -> fi


	