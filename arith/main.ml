open Format
open Support.Error
open Support.Pervasive
open Syntax
open Core

let searchpath = ref [""]

let argspecs = [
  "-I",
  Arg.String (fun f -> searchpath := f :: !searchpath),
  "Append a directory to the search path"
]

(* オプション引数を処理し，非オプション引数をそのまま返す *)
let parseArgs () =
	let inFile = ref (None : string option) in
  Arg.parse argspecs 
		(fun s ->
			match !inFile with
			| Some _ -> err "You must specify exactly one input file" (* 非オプション引数が2つ以上あるとき *)
			| None -> inFile := Some s) "";
	match !inFile with
	| None -> err "You must specify exactly one input file" (* 非オプション引数が1つもないとき *)
	| Some s -> s

(* infile を searchpath から探し出して， in_channel を開いて返す *)
let openfile infile =
	let rec trynext = function
	| [] -> err ("Couldn't find " ^ infile)
	| d :: rest ->
			let fname = if d = "" then infile else (d ^ "/" ^ infile) in
			try open_in fname with
			| Sys_error m -> trynext rest
	in trynext !searchpath

(* inFile を受け取り，それを読み込み AST にして返す *)
let parseFile inFile =
	let file_ch = openfile inFile in
	(* lexer.ml の関数を呼び出す．info で使うファイル名はここで渡す *)
	let lexbuf = Lexer.create inFile file_ch in
	let result =
		try Parser.toplevel Lexer.main lexbuf with
		| Parsing.Parse_error -> error (Lexer.info lexbuf) "Parse error"
	in
	Parsing.clear_parser ();
	close_in file_ch;
	result

(* 重複した import コマンドは省けるように管理する *)
let alreadyImported = ref ([] : string list)

(* ファイルの処理とコマンドの処理 *)
let rec process_file f =
	if List.mem f (!alreadyImported) then ()
	else (
		alreadyImported := f :: !alreadyImported;
		let cmds = parseFile f in
		List.iter process_cmd cmds
	)
and process_cmd = function
	| Import file -> process_file file
	| Eval (fi, t) ->
			let v = eval t in
			printtm v

let main () =
	let inFile = parseArgs () in
	process_file inFile

let () = set_max_boxes 1000
let () = set_margin 67


let _ =
	let res =
		Printexc.print (fun () ->
			try main (); 0 with
			| Exit x -> x) ()
	in
	print_flush ();
	exit res
