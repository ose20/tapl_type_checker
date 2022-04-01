open Letexercise
open Support.Error
open Syntax

let search_path = ref [""]

let arg_defs = [
  "-I",
  Arg.String (fun dir -> search_path := dir :: !search_path),
  "Append a directory to the search path"
]

let parse_args () =
  let file_name = ref (None : string option) in
  Arg.parse arg_defs
  (fun s -> match !file_name with
    | Some _ -> errs "You must specify one input file"
    | None -> file_name := Some s)
  "";
  match !file_name with
    | None -> errs "You must specify an input file"
    | Some s -> s

let open_file in_file =
  let rec try_next = function
    | [] -> errs @@ "Could not fine" ^ in_file
    | (d::rest) ->
        let name = if d = "" then in_file else (d ^ "/" ^ in_file) in
        try open_in name with
          | Sys_error _ -> try_next rest
    in try_next !search_path

let parse_file in_file =
  let in_ch = open_file in_file in
  let lexbuf = Lexer.create in_file in_ch in
  let result =
    try Parser.toplevel Lexer.main lexbuf with
      | Parsing.Parse_error -> errs_at (Lexer.info lexbuf) "Parse error"
  in
  Parsing.clear_parser(); close_in in_ch; result

let proc_cmd ctx = function
  | Eval (_, t) ->
      let t' = eval t in
      print_tmty ctx t'


let process_file file ctx =
  let (cmds, _) = parse_file file ctx in
  List.iter (proc_cmd ctx) cmds


let main () = 
  let file_name = parse_args() in
  process_file file_name empty_context


let () = exit @@
  try main(); 0 with
    | Exit x -> x