open Format
open Support.Error
open Support.Pervasive
open Syntax
open Core

(* ----------------------------------------------------------------------- *)
(* mutable cell *)

let searchpath = ref [""]
let alreadyImported = ref ([] : string list)

(* ----------------------------------------------------------------------- *)


let argdef = [
	"-I",
	Arg.String (fun f -> searchpath := f :: !searchpath),
	"Append a dirctory to the search paht"
]

let parseArgs () =
	let file = ref (None : string option) in
	Arg.parse argdef (fun s ->
		match !file with
		| Some _ -> errs "You must specify exactly one input file"
		| None -> file := Some s) "";
	match !file with
	| None -> errs "You must specify an input file"
	| Some s -> s

let openfile file =
	let rec trynext = function
		| [] -> errs ("Could not fine " ^ file)
		| d :: rest ->
				let path = if d = "" then file else (d ^ "/" ^ file) in
				try open_in path
				with Sys_error _ -> trynext rest
	in trynext !searchpath

let parseFile file =
	let in_ch = openfile file in
	let lexbuf = Lexer.createbuf file in_ch in
	let res =
		try Parser.toplevel Lexer.main lexbuf
		with Parsing.Parse_error -> errsAt (Lexer.info lexbuf) "Parse error"
	in Parsing.clear_parser (); close_in in_ch; res


let rec processFile file ctx =
	if List.mem file !alreadyImported then ctx else
	let (cmds, _) = parseFile file ctx in
	List.fold_left (fun ctx cmd -> processCommand ctx cmd) ctx cmds
and processCommand ctx = function
	| Import(f) ->
			let _ = processFile f ctx in ctx
	| Eval(fi, t) ->
			let t' = eval t in
			printtm ctx t'; ctx
	| Bind(fi, str) ->
			printf "@[<hov 0>Bind %s;@]@." str;
			addname ctx str


let () = set_max_boxes 1000
let () = set_margin 67

let main () =
	let file = parseArgs () in
	let _ = processFile file emptycontext in
	()

let res =
  Printexc.print (fun () ->
		try main (); 0
		with Exit x -> x)
	()
let () = print_flush (); exit res