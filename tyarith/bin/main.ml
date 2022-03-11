open Tyarith
open Format
open Support.Pervasive
open Support.Error
open Syntax
open Core


let searchpath = ref [""]

let argDefs = [
  "-I",
  Arg.String (fun f -> searchpath := f :: !searchpath),
  "Append a directory to the search path"
]

(* オプション引数の処理をする．その後，非オプションファイルの形式をチェックし，正常ならばそのまま返す *)
let parseArgs () = 
  let inFile = ref (None : string option) in
  Arg.parse argDefs
  (fun s ->
    match !inFile with
      | Some(_) -> err "You must specify exactry one input file"
      | None -> inFile := Some(s)) 
  "";
  match !inFile with
    | None -> err "You must specify an input file"
    | Some(s) -> s

let openfile infile =
  let rec trynext l = match l with
    | [] -> err ("Could not find " ^ infile)
    | (d::rest) ->
        let name = if d = "" then infile else (d ^ "/" ^ infile) in
        try open_in name
        with Sys_error m -> trynext rest
  in trynext !searchpath

let parseFile inFile = 
  let pi = openfile inFile in
  let lexbuf = Lexer.create inFile pi in
  let result = 
    try Parser.toplevel Lexer.main lexbuf with 
      |Parsing.Parse_error -> error (Lexer.info lexbuf) "Parse error"
  in
  Parsing.clear_parser(); close_in pi; result


let alreadyImported = ref ([] : string list)

let rec process_file f =
  if List.mem f (!alreadyImported) then ()
  else (
    alreadyImported := f :: !alreadyImported;
    let cmds = parseFile f in
    let g c =
      open_hvbox 0;
      let res = process_command c in
      print_flush ();
      res
    in List.iter g cmds
  )
and process_command = function
  | Import(f) -> process_file f
  | Eval(fi, t) ->
      open_hovbox 0;
      let tyT = typeof t in
      let t' = eval t in
      printtm t';
      print_break 1 2;
      pr ": ";
      printty tyT;
      print_newline ();
      close_box ()

let main () = 
  let inFile = parseArgs () in 
  process_file inFile

let () = set_max_boxes 1000
let () = set_margin 67

let _ =
  let res = Printexc.catch (fun () ->
    try main (); 0
  with Exit x -> x) ()
  in
  print_flush ();
  exit res
