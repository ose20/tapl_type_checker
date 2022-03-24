open Simplebool
open Format
open Support.Pervasive
open Support.Error
open Syntax

let search_path = ref [""]
let already_imported = ref ([] : string list)

let arg_defs = [
  "-I",
  Arg.String (fun f -> search_path := f :: !search_path),
  "Append a directory to the search path"
]

let parse_args () =
  let filename = ref (None : string option) in
  Arg.parse arg_defs
  (fun s -> match !filename with
    | Some _ -> errs "You must specify exactly one input file"
    | None -> filename := Some s)
  "";
  match !filename with
    | None -> errs "You must specify an input file"
    | Some s -> s

let prbindingty = function
  | NameBind -> ()
  | VarBind(tyT) -> pr ": "; printty tyT

let open_file in_file =
  let rec try_next = function
    | [] -> errs @@ "Could not find " ^ in_file
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

let rec process_command ctx = function
  | Import(f) ->
      process_file f ctx
  | Eval(_,t) ->
      let tyT = typeof ctx t in
      let t' = eval t in
      open_hvbox 0;
      printtm ctx t';
      break 1 2;
      pr ": ";
      printty tyT;
      print_newline();
      close_box();
      ctx
  | Bind(_,x,bind) ->
      open_hvbox 0;
      let (_, x') = pick_fresh_name ctx x in
      pr x'; pr " "; prbindingty bind;
      print_newline();
      close_box();
      let ctx' = add_binding ctx x' bind in
      print_endline @@ string_of_int @@ ctx_len ctx';
      ctx'

and process_file f ctx =
  if List.mem f (!already_imported) then ctx
  else
    begin
      already_imported := f :: !already_imported;
      let cmds, _ = parse_file f ctx in
      List.fold_left (fun ctx cmd -> process_command ctx cmd) ctx cmds
    end

let main () =
  let filename = parse_args() in
  let _ = process_file filename empty_context in ()

let () = exit @@
  try main(); 0 with
  | Exit x -> x
